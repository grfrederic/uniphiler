:- module(llvm, [compile_to_llvm/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

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


program_llvm(AST) --> sequence(topDef, AST).


% === TOP LEVEL DEF ===

topDef(def(Id, RetType, Args, Body, _Loc)) -->
    { function_context_init(Args, Cont),
      phrase(blck(Body, Cont, _ContNext), BodyLLVM) },
    [func(RetType, Id, Args, BodyLLVM)].


function_context_init(Args, [Map]) :-
    maplist(arg_to_reg, Args, Map).

arg_to_reg((Id, Type), (Id, (Type, _FreshReg))).


% === BLOCK ===

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
    stmt(assgStmt(I, epls(expr_id(I), expr_int(1)), Loc), Cont, ContNext).

stmt(decrStmt(I, Loc), Cont, ContNext) --> !,
    stmt(assgStmt(I, emin(expr_id(I), expr_int(1)), Loc), Cont, ContNext).

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
stmt(condStmt(E, ST, SF, _Loc), Cont, ContNext) --> !,
    expression(E, Cont, Out),
    [br(Out, LabelTrue, LabelFalse)],
    [label(LabelTrue)],
    stmt(ST, Cont, ContTrue),
    [br(LabelEnd)],
    [label(LabelFalse)],
    stmt(SF, Cont, ContFalse),
    [label(LabelEnd)],
    phi_cond(ContTrue, LabelTrue, ContFalse, LabelFalse, Cont, ContNext).

% while
stmt(whilStmt(_E, _S, _Loc), Cont, Cont) --> !, [while].

% expr
stmt(exprStmt(E, _Loc), Cont, Cont) --> !, expression(E, Cont, _Out).


% === PHI CORRECTIONS FOR COND AND WHILE ===
phi_cond([], _LT, [], _LF, [], []) --> !.
phi_cond([MT|CT], LT, [MF|CF], LF, [M|C], [ME|CE]) -->
    phi_cond_map(M, MT, LT, MF, LF, ME),
    phi_cond(CT, LT, CF, LF, C, CE).

phi_cond_map([], _MT, _LT, _MF, _LF, []) --> !, [].
phi_cond_map([(Var, _)|M], MT, LT, MF, LF, [(Var, NewVal)|ME]) -->
    { member((Var, VT), MT),
      member((Var, VF), MF),
      ! },
    (   { VT == VF }
    ->  { NewVal = VT }
    ;   [phi(NewVal, [(VT, LT), (VF, LF)])]
    ),
    phi_cond_map(M, MT, LT, MF, LF, ME).



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
    { E =.. [Op, Type, E1, E2],
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
    [call(Out, str, "freaky_string_comp_call", [O1, O2])].

% built in ops
expression(E, Cont, Out) -->
    { E =.. [Op, Type, E1, E2],
      op_type_llvm_op(Op, Type, LlvmOp) },
    !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    { LlvmLine =.. [LlvmOp, Out, Type, [O1, O2]] },
    [LlvmLine].

% runtime ops
expression(epls(str, E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, str, "some_weird_string_call", [O1, O2])].


expression(enot(_Type, E), Cont, Out) --> !,
    expression(eeq(expr_bool(boolean, false), E), Cont, Out).

expression(eneg(Type, E), Cont, Out) --> !,
    expression(emin(Type, expr_int(int, 0), E), Cont, Out).


expression(expr_int(_Type, I), _Cont, I) --> !.
expression(expr_str(_Type, S), _Cont, S) --> !. % TODO(frdrc): some global stuff
expression(expr_bool(_Type, B), _Cont, B) --> !.


expression(expr_ap(Type, I, Es), Cont, Out) --> !,
    expressions_types_outs(Es, Cont, TypesOuts),
    [call(Out, Type, I, TypesOuts)].

expression(expr_id(Type, I), Cont, Out) --> !,
    {context_get(Cont, I, (Type, Out))}.

expression(expr_in(_Type, E), Cont, Out) --> !,
    expression(E, Cont, Out).


expression(_E, _Cont, _Out) --> [some_computation].  % TODO(frdrc): this should not be necessary


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
cmp_llvm_icmp(elt, sgt).


% op_type_llvm_op(?Op, ?Type, ?LlvmOp).
op_type_llvm_op(emod, int, srem).
op_type_llvm_op(ediv, int, sdiv).
op_type_llvm_op(eprd, int, mul).
op_type_llvm_op(epls, int, add).
op_type_llvm_op(emin, int, sub).
op_type_llvm_op(eor, boolean, or).
op_type_llvm_op(eand, boolean, and).


% === UTILS ===

% t(+Expr, ?Type)
t(Expr, Type) :- Expr =.. [_, Type|_].
