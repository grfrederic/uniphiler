:- module(typing_and_checks, [check_and_derive_types/1]).


:- use_module('errors.prolog').
:- use_module('context.prolog').
:- use_module('simplify.prolog').


check_and_derive_types(AST) :-
    check_all_args_different(AST),
    check_typing_and_declarations(AST), !.

check_and_derive_types(_) :-
    error_stack_print,
    error_stack_clear,
    fail.


% === ARGS DIFFERENT ===

check_all_args_different([]).
check_all_args_different([def(Id, _RetType, Args, _Body, Loc)|AST]) :-
    complain_on_fail(["in top level definition of", Id], Loc),
    all_args_different(Args),
    !,
    check_all_args_different(AST).

all_args_different([]) :- !.
all_args_different(Args) :-
    maplist(extract_arg_name, Args, ArgNamesBag),
    sort(ArgNamesBag, ArgNamesSet),
    same_length(ArgNamesSet, ArgNamesBag), !.

all_args_different(_) :-
    error("argument names have duplicates").

extract_arg_name((ArgName, _), ArgName).
extract_arg_type((_, ArgType), ArgType).


% === TYPING - CHECK AST ===

check_typing_and_declarations(AST) :-
    maplist(extract_def_type, AST),
    type_check_main,
    maplist(type_check_def, AST),
    retractall(declared_function(_, _, _)).


:- dynamic('declared_function'/3).
extract_def_type(def(Id, RetType, Args, _Body, _Loc)) :-
    maplist(extract_arg_type, Args, ArgTypes),
    ( declared_function(Id, _, _), !, error(["redaclaration of", Id])
    ; assertz(declared_function(Id, RetType, ArgTypes))
    ).


type_check_main :-
    complain_on_fail("main not declared"),
    declared_function("main", RetType, _ArgTypes), !,
    ( RetType = int, !
    ; error("main should return 'int'")
    ).


type_check_def(def(Id, RetType, Args, Body, Loc)) :-
    complain_on_fail(["in top level definition of", Id], Loc),
    type_check_blck(Body, RetType, SurelyReturns, [Args]),
    ( SurelyReturns = 1, !
    ; RetType = void, !
    ; error("control flow exits without return")
    ).


type_check_blck(blck(Stmts, Loc), RetType, SurelyReturns, ContOuter) :-
    complain_on_fail("in block", Loc),
    context_sub(ContOuter, Cont),
    type_check_blck_stmts(Stmts, RetType, SurelyReturns, Cont).

type_check_blck_stmts([Stmt|Stmts], RetType, BlockSurelyReturns, Cont) :-
    type_check_stmt(Stmt, RetType, SurelyReturns, Cont, ContNext), !,
    (   SurelyReturns = 1
    ->  BlockSurelyReturns = 1
    ;   type_check_blck_stmts(Stmts, RetType, BlockSurelyReturns, ContNext)
    ).

type_check_blck_stmts([], _, 0, _).


type_check_stmt(blckStmt(Block), RetType, SurelyReturns, Cont, Cont) :- !,
    type_check_blck(Block, RetType, SurelyReturns, Cont).

type_check_stmt(incrStmt(I, Loc), _RetType, 0, Cont, Cont) :- !,
    complain_on_fail([I, "should be of type 'int' in statement"], Loc),
    context_get(Cont, I, int).

type_check_stmt(decrStmt(I, Loc), _RetType, 0, Cont, Cont) :- !,
    complain_on_fail([I, "should be of type 'int' in statement"], Loc),
    context_get(Cont, I, int).

type_check_stmt(declStmt(Type, Items, Loc), _RetType, 0, Cont, ContNext) :- !,
    complain_on_fail("in declaration", Loc),
    context_add_items(Items, Type, Cont, ContNext).

type_check_stmt(assgStmt(Id, Expr, Loc), _RetType, 0, Cont, Cont) :- !,
    complain_on_fail("in assignment", Loc),
    context_type_expr(Cont, ExprType, Expr),
    context_get(Cont, Id, Type),
    ( Type = ExprType
    ; error(["expression for", Id, "should be", Type, "but is", ExprType])
    ).

type_check_stmt(condStmt(E, ST, SF, Loc), RetType, SurelyReturns, Cont, Cont) :- !,
    complain_on_fail("in if-statement", Loc),
    context_type_expr(Cont, Type, E),
    ( Type = boolean
    ; error(["'if' condition should evaluate to boolean but is", Type])
    ),
    type_check_stmt(ST, RetType, SurelyReturnsTrue, Cont, _ContTrue),
    type_check_stmt(SF, RetType, SurelyReturnsFalse, Cont, _ContFalse),
    (   evaluate_trivial(E, Val)
    ->  !,
        (   Val = true
        ->  SurelyReturns = SurelyReturnsTrue
        ;   SurelyReturns = SurelyReturnsFalse
        )
    ;   SurelyReturns is SurelyReturnsTrue * SurelyReturnsFalse
    ).

type_check_stmt(whilStmt(E, S, Loc), RetType, SurelyReturns, Cont, ContNext) :- !,
    complain_on_fail("in while-statement", Loc),
    context_type_expr(Cont, Type, E),
    ( Type = boolean
    ; error(["'while' condition should evaluate to boolean but is", Type])
    ),
    type_check_stmt(S, RetType, SurelyReturnsLoop, Cont, ContLoop),
    (   evaluate_trivial(E, true)
    ->  SurelyReturns = SurelyReturnsLoop, ContNext = ContLoop
    ;   SurelyReturns = 0, ContNext = Cont
    ).

type_check_stmt(rtrnStmt(Loc), RetType, 1, Cont, Cont) :- !,
    complain_on_fail("in return", Loc),
    complain_on_fail(["should return", RetType, "but tried to return 'void'"], Loc),
    void = RetType.

type_check_stmt(rtrnStmt(E, Loc), RetType, 1, Cont, Cont) :- !,
    complain_on_fail("in return", Loc),
    context_type_expr(Cont, Type, E),
    complain_on_fail(["should return", RetType, "but tried to return", Type], Loc),
    Type = RetType.

type_check_stmt(exprStmt(E, Loc), _RetType, 0, Cont, Cont) :- !,
    complain_on_fail("in expression", Loc),
    context_type_expr(Cont, _Type, E).


type_check_stmt(emptStmt, _RetType, 0, Cont, Cont).


% context_add_items(+Items, +Type, +Cont, -ContFinal)
context_add_items([lit(I)|Items], Type, Cont, ContFinal) :-
    !,
    context_insert(Cont, I, Type, ContNext),
    context_add_items(Items, Type, ContNext, ContFinal).

context_add_items([ass(I, E)|Items], Type, Cont, ContFinal) :-
    !,
    context_insert(Cont, I, Type, ContNext),
    context_type_expr(ContNext, ExprType, E),
    complain_on_fail(["expression for", I, "should be", Type, "but is", ExprType]),
    Type = ExprType,
    context_add_items(Items, Type, ContNext, ContFinal).

context_add_items([], _, Cont, Cont).


% === TYPING ===

context_type_expr(Cont, Type, Expr) :- etc_(Expr, Type, Cont).

% for these don't check args
etc_(expr_id(Type, I), Type, Cont) :- !, context_get(Cont, I, Type).
etc_(expr_str(str, _), str, _) :- !.
etc_(expr_int(int, _), int, _) :- !.
etc_(expr_bool(boolean, _), boolean, _) :- !.

etc_(expr_ap(Type, FuncId, Args), Type, Cont) :- !,
    get_declared_function(FuncId, Type, FuncArgTypes),
    etc_map_(Args, ArgTypes, Cont),
    args_match(FuncId, FuncArgTypes, ArgTypes).

etc_(Expr, Type, Cont) :-
    Expr =.. [Func, Type|Args],
    etc_map_(Args, ArgTypes, Cont),
    ( registered_functions(Func, _, Type, ArgTypes, Cont), !
    ; registered_functions(Func, FuncName, Type, FuncArgTypes, Cont),
      args_match(FuncName, FuncArgTypes, ArgTypes)
    ).

etc_map_([E|Es], [T|Ts], Cont) :-
    !,
    etc_(E, T, Cont),
    etc_map_(Es, Ts, Cont).

etc_map_([], [], _).


get_declared_function("readInt", int, []) :- !.
get_declared_function("readString", str, []) :- !.
get_declared_function("printInt", void, [int]) :- !.
get_declared_function("printString", void, [str]) :- !.

get_declared_function(FuncId, RetType, ArgTypes) :-
    declared_function(FuncId, RetType, ArgTypes), !.

get_declared_function(FuncId, _RetType, _ArgTypes) :-
    error(["function '", FuncId, "' not declared"]).


registered_functions(expr_in, "()", Alpha, [Alpha], _).

registered_functions(eneg, "-", int, [int], _).
registered_functions(enot, "!", boolean, [boolean], _).

registered_functions(eor, "||", boolean, [boolean, boolean], _).
registered_functions(eand, "&&", boolean, [boolean, boolean], _).

registered_functions(ene, "!=", boolean, [Alpha, Alpha], _).
registered_functions(eeq, "==", boolean, [Alpha, Alpha], _).
registered_functions(ele, "<=", boolean, [Alpha, Alpha], _).
registered_functions(ege, ">=", boolean, [Alpha, Alpha], _).
registered_functions(elt, "<", boolean, [Alpha, Alpha], _).
registered_functions(egt, ">", boolean, [Alpha, Alpha], _).

registered_functions(emod, "%", int, [int, int], _).
registered_functions(ediv, "/", int, [int, int], _).
registered_functions(eprd, "*", int, [int, int], _).
registered_functions(epls, "+", int, [int, int], _).
registered_functions(epls, "+", str, [str, str], _).
registered_functions(emin, "-", int, [int, int], _).

registered_functions(F, _, _, _) :-
    error(["couldnt match type for function:", F]).


args_match(FuncName, As, Bs) :-
    length(As, An), length(Bs, Bn),
    ( An = Bn, !
    ; error(["function", FuncName, "expects", An, "arguments but got", Bn])
    ),
    args_match_(As, Bs, 1, FuncName).

args_match_([], [], _N, _FuncName).
args_match_([A|As], [B|Bs], N, FuncName) :-
    complain_on_fail(["argument", N, "of", FuncName, "should be of type", A, "but got", B]),
    A = B, !,
    N1 is N + 1,
    args_match_(As, Bs, N1, FuncName).



% === UTILS ===

same_length([], []).
same_length([_|L1], [_|L2]) :- same_length(L1, L2).
