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

arg_to_reg((Id, _Type), (Id, _Reg)).


% === BLOCK ===

blck(blck([], _Loc), Cont, Cont) --> [].

blck(blck([Stmt|Stmts], Loc), Cont, ContNext) -->
    stmt(Stmt, Cont, ContStep),
    blck(blck(Stmts, Loc), ContStep, ContNext).


% === STMT ===

stmt(emptStmt, Cont, Cont) --> "".

stmt(blckStmt(Block), Cont, ContNext) -->
    {context_sub(Cont, ContInner)},
    blck(Block, ContInner, [_|ContNext]).  % drop inner vars

% IncrStmt / DecrStmt
stmt(incrStmt(I, Loc), Cont, ContNext) -->
    stmt(assgStmt(I, epls(expr_id(I), expr_int(1)), Loc), Cont, ContNext).

stmt(decrStmt(I, Loc), Cont, ContNext) -->
    stmt(assgStmt(I, emin(expr_id(I), expr_int(1)), Loc), Cont, ContNext).

% DeclStmt
stmt(declStmt(_Type, [], _Loc), Cont, Cont) --> [].
stmt(declStmt(Type, [Item|Items], Loc), Cont, ContNext) -->
    item(Type, Item, Cont, ContStep),
    stmt(declStmt(Type, Items, Loc), ContStep, ContNext).

% AssStmt
stmt(assgStmt(I, E, _Loc), Cont, ContNext) -->
    expression(E, Cont, Out),
    {context_update(Cont, I, Out, ContNext)}.

% RetStmt / VRetStmt
stmt(rtrnStmt(E, _Loc), Cont, Cont) --> expression(E, Cont, Out), [return(Out)].
stmt(rtrnStmt(_Loc), Cont, Cont) --> [return].

% if
stmt(condStmt(_E, _ST, _SF, _Loc), Cont, Cont) --> [cond].

% while
stmt(whilStmt(_E, _S, _Loc), Cont, Cont) --> [while].

% expr
stmt(exprStmt(E, _Loc), Cont, Cont) --> expression(E, Cont, _Out).


% === INTRODUCING NEW VARS ===

item(int, lit(I), Cont, ContNext) --> !, {context_insert(Cont, I, 0, ContNext)}.
item(str, lit(I), Cont, ContNext) --> !, {context_insert(Cont, I, "", ContNext)}.
item(_, lit(_), _, _) --> {error("initialization error")}.
item(_, ass(I, E), Cont, ContNext) --> 
    expression(E, Cont, Out),
    {context_insert(Cont, I, Out, ContNext)}.


% === EXPRESSIONS ===

expression(eor(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "or", [O1, O2])].

expression(eand(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "and", [O1, O2])].

expression(ene(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "ne", [O1, O2])].

expression(eeq(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "eq", [O1, O2])].

expression(ele(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "ele", [O1, O2])].

expression(ege(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "ege", [O1, O2])].

expression(elt(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "slt", [O1, O2])].

expression(egt(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "sgt", [O1, O2])].

expression(emod(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "srem", [O1, O2])].

expression(ediv(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "sdiv", [O1, O2])].

expression(eprd(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "mul", [O1, O2])].

expression(epls(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "add", [O1, O2])].

expression(emin(E1, E2), Cont, Out) --> !,
    expression(E1, Cont, O1),
    expression(E2, Cont, O2),
    [call(Out, "sub", [O1, O2])].


expression(enot(E), Cont, Out) --> !,
    expression(eeq(expr_bool(false), E), Cont, Out).

expression(eneg(E), Cont, Out) --> !,
    expression(emin(expr_int(0), E), Cont, Out).


expression(expr_int(I), _Cont, I) --> !.
expression(expr_str(S), _Cont, S) --> !. % TODO(frdrc): some global stuff
expression(expr_bool(B), _Cont, B) --> !.


expression(expr_ap(I, Es), Cont, Out) --> !,
    expressions(Es, Cont, Outs),
    [call(Out, I, Outs)].

expression(expr_id(I), Cont, Out) --> !,
    {context_get(Cont, I, Out)}.

expression(expr_in(E), Cont, Out) --> !,
    expression(E, Cont, Out).


expression(_E, _Cont, _Out) --> [some_computation].


% many at once with same context

expressions([], _Cont, []) --> [].
expressions([E|Es], Cont, [Out|Outs]) -->
    expression(E, Cont, Out),
    expressions(Es, Cont, Outs).
