:- module(llvm, [compile_to_llvm/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module('simplify.prolog').
:- use_module('errors.prolog').
:- use_module('context.prolog').
:- use_module('llvm_print.prolog').


compile_to_llvm(AST, LlvmStr) :-
    phrase(program_llvm(AST), Llvm), !,
    phrase(llvm_print(Llvm), LlvmCodes), !,
    string_codes(LlvmStr, LlvmCodes).


compile_to_llvm(_, _) :-
    error_stack_print,
    error_stack_clear,
    fail.


program_llvm(AST) -->
    { phrase(sequence(topDef, AST), Llvm),
      split(Llvm, LlvmFuncs, LlvmConsts)
    },
    LlvmConsts, LlvmFuncs.


% === MOVE GLOBALS TO TOP ===

split(FuncWithConsts, Funcs, Consts) :-
    maplist(split_func, FuncWithConsts, Funcs, ConstLists),
    flatten(ConstLists, Consts).

split_func(func(RetType, Id, Regs, LinesWithConsts),
           func(RetType, Id, Regs, Lines),
           Consts) :-
    exclude(is_const, LinesWithConsts, Lines),
    include(is_const, LinesWithConsts, Consts).

is_const(constant(_, _, _)).


% === TOP LEVEL DEF ===
topDef(def(Id, RetType, Args, Body, _Loc)) -->
    { function_context_and_regs_init(Args, Cont, Regs),
      phrase(blck_cut(Body, Cont, _ContNext), BodyLLVM) }, !,
    [func(RetType, Id, Regs, BodyLLVM)].


function_context_and_regs_init(Args, [Map], Regs) :-
    maplist(arg_to_mapping_and_reg, Args, Map, Regs).

arg_to_mapping_and_reg((Id, Type), (Id, (Type, FreshReg)), (Type, FreshReg)).


% === BLOCK ===

blck_cut(Body, Cont, ContNext) --> blck(Body, Cont, ContNext), !.

blck(blck([], _Loc), Cont, Cont) --> [].

blck(blck([Stmt|Stmts], Loc), Cont, ContNext) -->
    stmt(Stmt, Cont, ContStep),
    blck(blck(Stmts, Loc), ContStep, ContNext).


% === STMT ===

stmt(emptStmt, Cont, Cont) --> !, [].

stmt(blckStmt(Block), Cont, ContNext) --> !,
    {context_sub(Cont, ContInner)},
    blck(Block, ContInner, [_|ContNext]).  % drop inner vars

% IncrStmt / DecrStmt
stmt(incrStmt(I, Loc), Cont, ContNext) --> !,
    stmt(assgStmt(I, epls(int, expr_id(int, I), expr_int(int, 1)), Loc), Cont, ContNext).

stmt(decrStmt(I, Loc), Cont, ContNext) --> !,
    stmt(assgStmt(I, emin(int, expr_id(int, I), expr_int(int, 1)), Loc), Cont, ContNext).

% DeclStmt
stmt(declStmt(_Type, [], _Loc), Cont, Cont) --> !, [].
stmt(declStmt(Type, [Item|Items], Loc), Cont, ContNext) --> !,
    item(Type, Item, Cont, ContStep),
    stmt(declStmt(Type, Items, Loc), ContStep, ContNext).

% AssStmt
stmt(assgStmt(I, E, _Loc), Cont, ContNext) --> !,
    expression(E, Cont, Out),
    {t(E, Type), context_update(Cont, I, (Type, Out), ContNext)}.

% RetStmt / VRetStmt
stmt(rtrnStmt(E, _Loc), Cont, Cont) --> !, {t(E, Type)}, expression(E, Cont, Out), [return(Type, Out)].
stmt(rtrnStmt(_Loc), Cont, Cont) --> !, [return].

% if
stmt(condStmt(E, ST, _SF, _Loc), Cont, ContNext) -->
    { evaluate_trivial(E, true) }, !,
    stmt(ST, Cont, ContNext).

stmt(condStmt(E, _ST, SF, _Loc), Cont, ContNext) -->
    { evaluate_trivial(E, false) }, !,
    stmt(SF, Cont, ContNext).

stmt(condStmt(E, ST, SF, _Loc), Cont, ContNext) --> !,
    expression(E, Cont, Out),
    [br(Out, LabelTrue, LabelFalse)],

    % if true
    [label(LabelTrue)],
    stmt(ST, Cont, ContTrue),
    [br(LabelTrueEnd), label(LabelTrueEnd)], % set ancestor for phi
    [br(LabelEnd)],

    % if false
    [label(LabelFalse)],
    stmt(SF, Cont, ContFalse),
    [br(LabelFalseEnd), label(LabelFalseEnd)], % set ancestor for phi
    [br(LabelEnd)],

    % exit
    [label(LabelEnd)],
    phi_merge(ContTrue, LabelTrueEnd, ContFalse, LabelFalseEnd, Cont, ContNext).

% while
stmt(whilStmt(E, Body, _Loc), Cont, ContShld) --> !,
    { shield_changing_vars(Body, Cont, ContShld),
      phrase(expression(E, ContShld, Out), LlvmCond), !,
      phrase(stmt(Body, ContShld, ContBody), LlvmBody), !,
      phrase(phi_merge(Cont, LabelEntry, ContBody, LabelBodyEnd, Cont, ContShld), LlvmRephi), !
    },
    [br(LabelEntry), label(LabelEntry)],  % set ancestor for phi
    [br(LabelRephi)],

    [label(LabelRephi)],  % for looping

    % sync entry and loop
    LlvmRephi,
    % check cond
    LlvmCond,
    [br(Out, LabelBody, LabelEnd)],

    % body
    [label(LabelBody)],
    LlvmBody,
    [br(LabelBodyEnd), label(LabelBodyEnd)],  % set ancestor for phi
    [br(LabelRephi)],

    % exit
    [label(LabelEnd)].


% expr
stmt(exprStmt(E, _Loc), Cont, Cont) --> !, expression(E, Cont, _Out).


% === PHI CORRECTIONS FOR MERGING PATHS ===

phi_merge([], _LT, [], _LF, [], []) --> !.
phi_merge([MT|CT], LT, [MF|CF], LF, [M|C], [ME|CE]) -->
    phi_merge_map(M, MT, LT, MF, LF, ME),
    phi_merge(CT, LT, CF, LF, C, CE).

phi_merge_map([], _MT, _LT, _MF, _LF, []) --> !, [].
phi_merge_map([(Var, (Type, _))|M], MT, LT, MF, LF, [(Var, (Type, NewVal))|ME]) -->
    { member((Var, VT), MT),
      member((Var, VF), MF),
      ! },
    (   { VT == VF }
    ->  { (Type, NewVal) = VT }
    ;   [phi((Type, NewVal), [(VT, LT), (VF, LF)])]
    ),
    phi_merge_map(M, MT, LT, MF, LF, ME).



% === SHIELD CHANGING VARS ===
shield_changing_vars(Body, Cont, ContShld) :-
    phrase(stmt(Body, Cont, ContBody), _), !,
    shield_changed(Cont, ContBody, ContShld).

shield_changed(Cont, ContBody, ContShld) :-
    maplist(shield_changed_map, Cont, ContBody, ContShld).

shield_changed_map([], _, []) :- !.
shield_changed_map([(Var, Val)|M], Chgd, [(Var, ShldVal)|Shld]) :- !,
    member((Var, ChgdVal), Chgd), !,
    (   Val == ChgdVal
    ->  ShldVal = Val
    ;   true
    ),
    shield_changed_map(M, Chgd, Shld).



% === INTRODUCING NEW VARS ===

item(int, lit(I), Cont, ContNext) --> !, {context_insert(Cont, I, (int, 0), ContNext)}.
item(str, lit(I), Cont, ContNext) --> !, {context_insert(Cont, I, (str, ""), ContNext)}.
item(_, lit(_), _, _) --> {error("initialization error")}.
item(_, ass(I, E), Cont, ContNext) --> 
    expression(E, Cont, Out),
    {t(E, Type), context_insert(Cont, I, (Type, Out), ContNext)}.


% === EXPRESSIONS ===

% built in compares
expression(E, Cont, Out) -->
    { E =.. [Op, _Type, E1, E2],
      t(E1, Type),
      cmp_llvm_icmp(Op, LlvmCond),
      member(Type, [int, boolean]) },
    !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    { LlvmLine =.. [icmp, Out, LlvmCond, Type, [O1, O2]] },
    [LlvmLine].

% runtime compares
expression(E, Cont, Out) -->
    { E =.. [Op, str, E1, E2],
      cmp_llvm_icmp(Op, _LlvmCond) },
    !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, str, "compareStrings", [(str, O1), (str, O2)])].

% built in ops
expression(E, Cont, Out) -->
    { E =.. [Op, Type, E1, E2],
      op_type_llvm_op(Op, Type, LlvmOp) },
    !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    { LlvmLine =.. [LlvmOp, Out, Type, [O1, O2]] },
    [LlvmLine].

% binary ops
expression(eor(boolean, E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    [br(O1, LabelWin, LabelRetry)],
    [label(LabelRetry)],
    expression(E2, Cont, O2),
    [br(O2, LabelWin, LabelFail)],
    [label(LabelWin)],
    [br(LabelFin)],
    [label(LabelFail)],
    [br(LabelFin)],
    [label(LabelFin)],
    [phi((boolean, Out), [((boolean, true), LabelWin),
                          ((boolean, false), LabelFail)])].


expression(eand(boolean, E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    [br(O1, LabelHalf, LabelFail)],
    [label(LabelHalf)],
    expression(E2, Cont, O2),
    [br(O2, LabelWin, LabelFail)],
    [label(LabelWin)],
    [br(LabelFin)],
    [label(LabelFail)],
    [br(LabelFin)],
    [label(LabelFin)],
    [phi((boolean, Out), [((boolean, true), LabelWin),
                          ((boolean, false), LabelFail)])].


% runtime ops
expression(epls(str, E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, str, "concatStrings", [(str, O1), (str, O2)])].


expression(enot(_Type, E), Cont, Out) --> !,
    expression(eeq(boolean, expr_bool(boolean, false), E), Cont, Out).

expression(eneg(Type, E), Cont, Out) --> !,
    expression(emin(Type, expr_int(int, 0), E), Cont, Out).


expression(expr_int(_Type, I), _Cont, I) --> !.
expression(expr_bool(_Type, B), _Cont, B) --> !.


expression(expr_str(_Type, S), _Cont, Out) --> !,
    { string_length(S, N),
      N1 is N + 1,
      string_concat(S, "\\00", S00)
    },
    [constant(C, arr(N1, char), S00)],
    [bitcast(Out, ptr(arr(N1, char)), C, str)].

expression(expr_ap(Type, I, Es), Cont, Out) --> !,
    expressions_types_outs(Es, Cont, TypesOuts),
    (   { Type = void }
    ->  [call(Type, I, TypesOuts)]
    ;   [call(Out, Type, I, TypesOuts)]
    ).

expression(expr_id(Type, I), Cont, Out) --> !,
    {context_get(Cont, I, (Type, Out))}.

expression(expr_in(_Type, E), Cont, Out) --> !,
    expression(E, Cont, Out).


% TODO(frdrc): this should not be necessary
expression(E, _Cont, _Out) --> { error(["could not compile expression", E]) }.


% many at once with same context

expressions_types_outs([], _Cont, []) --> [].
expressions_types_outs([E|Es], Cont, [(Type, Out)|TypesOuts]) -->
    expression(E, Cont, Out),
    { t(E, Type) },
    expressions_types_outs(Es, Cont, TypesOuts).


% === DISPATCH CMPS AND LLVM OPS ===

% cmp_llvm_icmp(?Op, ?LlvmOp).
cmp_llvm_icmp(eeq, eq).
cmp_llvm_icmp(ene, ne).
cmp_llvm_icmp(ele, sle).
cmp_llvm_icmp(ege, sge).
cmp_llvm_icmp(elt, slt).
cmp_llvm_icmp(egt, sgt).


% op_type_llvm_op(?Op, ?Type, ?LlvmOp).
op_type_llvm_op(emod, int, srem).
op_type_llvm_op(ediv, int, sdiv).
op_type_llvm_op(eprd, int, mul).
op_type_llvm_op(epls, int, add).
op_type_llvm_op(emin, int, sub).


% === UTILS ===

% t(+Expr, ?Type)
t(Expr, Type) :- Expr =.. [_, Type|_].
