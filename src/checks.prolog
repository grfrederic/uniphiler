:- module(checks, [all_checks/1]).


:- use_module('errors.prolog').
:- use_module('simplify.prolog').


all_checks(AST) :-
    check_all_args_different(AST),
    check_typing_and_declarations(AST).



% === ARGS DIFFERENT ===

check_all_args_different([]).
check_all_args_different([def(Id, _RetType, Args, _Body, Loc)|AST]) :-
    atomics_to_string(["in top level definition of '", Id, "'"], Msg),
    complain_on_fail(Msg, Loc),
    all_args_different(Args),
    !,
    check_all_args_different(AST).

all_args_different([]) :- !.
all_args_different(Args) :-
    maplist(extract_arg_name, Args, ArgNamesBag),
    sort(ArgNamesBag, ArgNamesSet),
    same_length(ArgNamesSet, ArgNamesBag), !.

all_args_different(_) :-
    write("argument names have duplicates"), nl, fail.

extract_arg_name((ArgName, _), ArgName).
extract_arg_type((_, ArgType), ArgType).


% === TYPING - CHECK AST ===

check_typing_and_declarations(AST) :-
    maplist(extract_def_type, AST),
    maplist(type_check_def, AST),
    retractall(declared_function(_, _, _)).


:- dynamic('declared_function'/3).
extract_def_type(def(Id, RetType, Args, _Body, _Loc)) :-
    maplist(extract_arg_type, Args, ArgTypes),
    assertz(declared_function(Id, RetType, ArgTypes)).


type_check_def(def(Id, RetType, Args, Body, Loc)) :-
    atomics_to_string(["in top level definition of '", Id, "'"], Msg),
    complain_on_fail(Msg, Loc),
    type_check_blck(Body, RetType, SurelyReturns, [Args]),
    (   SurelyReturns = 1
    ->  !, true
    ;   (   RetType = void
        ->  !, true
        ;   write("control flow exits without return"), nl, fail
        )
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
    atomics_to_string(["'", I, "' should be of type 'int' in statement"], Msg),
    complain_on_fail(Msg, Loc),
    context_get_type(Cont, I, int).

type_check_stmt(decrStmt(I, Loc), _RetType, 0, Cont, Cont) :- !,
    atomics_to_string(["'", I, "' should be of type 'int' in statement"], Msg),
    complain_on_fail(Msg, Loc),
    context_get_type(Cont, I, int).

type_check_stmt(declStmt(Type, Items, Loc), _RetType, 0, Cont, ContNext) :- !,
    complain_on_fail("in declaration", Loc),
    context_add_items(Items, Type, Cont, ContNext).

type_check_stmt(assgStmt(Id, Expr, Loc), _RetType, 0, Cont, Cont) :- !,
    complain_on_fail("in assignment", Loc),
    context_type_expr(Cont, ExprType, Expr),
    context_get_type(Cont, Id, Type),
    (   Type \= ExprType
    ->  write("expression for '"), write(Id),
        write("' should be '"), write(Type),
        write("' but is '"), write(ExprType),
        write("'"), nl, fail
    ;   true
    ).

type_check_stmt(condStmt(E, ST, SF, Loc), RetType, SurelyReturns, Cont, ContNext) :- !,
    complain_on_fail("in if-statement", Loc),
    context_type_expr(Cont, Type, E),
    (   Type \= boolean
    ->  write("'if' condition should evaluate to boolean but is "),
        write(Type), nl,
        fail
    ;   true
    ),
    type_check_stmt(ST, RetType, SurelyReturnsTrue, Cont, ContTrue),
    type_check_stmt(SF, RetType, SurelyReturnsFalse, Cont, ContFalse),
    (   evaluate_trivial(E, Val)
    ->  !,
        (   Val = true
        ->  ContNext = ContTrue, SurelyReturns = SurelyReturnsTrue
        ;   ContNext = ContFalse, SurelyReturns = SurelyReturnsFalse
        )
    ;   ContNext = Cont,  % TODO(frdrc): things declared in unclear if-else fall out of scope. good or not?
        SurelyReturns is SurelyReturnsTrue * SurelyReturnsFalse
    ).

type_check_stmt(whilStmt(E, S, Loc), RetType, SurelyReturns, Cont, ContNext) :- !,
    complain_on_fail("in while-statement", Loc),
    context_type_expr(Cont, Type, E),
    (   Type \= boolean
    ->  write("'while' condition should evaluate to boolean but is "),
        write(Type), nl,
        fail
    ;   true
    ),
    type_check_stmt(S, RetType, SurelyReturnsLoop, Cont, ContLoop),
    (   evaluate_trivial(E, true)
    ->  SurelyReturns = SurelyReturnsLoop, ContNext = ContLoop
    ;   SurelyReturns = 0, ContNext = Cont
    ).

type_check_stmt(rtrnStmt(Loc), RetType, 1, Cont, Cont) :- !,
    complain_on_fail("in return", Loc),
    (   void = RetType
    ->  true
    ;   write("should return '"), write(RetType),
        write("' but tried to return 'void'"),
        nl, fail
    ).

type_check_stmt(rtrnStmt(E, Loc), RetType, 1, Cont, Cont) :- !,
    complain_on_fail("in return", Loc),
    context_type_expr(Cont, Type, E),
    (   Type = RetType
    ->  true
    ;   write("should return '"), write(RetType),
        write("' but tried to return '"), write(Type), write("'"),
        nl, fail
    ).

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
    (   Type = ExprType
    ->  !, true
    ;   !,
        write("expression for '"), write(I),
        write("' should be '"), write(Type),
        write("' but is '"), write(ExprType),
        write("'"), nl, fail),
    context_add_items(Items, Type, ContNext, ContFinal).

context_add_items([], _, Cont, Cont).


% === TYPING ===

context_type_expr(Cont, Type, Expr) :- etc_(Expr, Type, Cont).

% for these don't check args
etc_(expr_id(I), Type, Cont) :- !, context_get_type(Cont, I, Type).
etc_(expr_str(_), str, _) :- !.
etc_(expr_int(_), int, _) :- !.
etc_(expr_bool(_), boolean, _) :- !.

etc_(expr_ap(FuncId, Args), Type, Cont) :- !,
    get_declared_function(FuncId, Type, FuncArgTypes),
    etc_map_(Args, ArgTypes, Cont),
    args_match(FuncId, FuncArgTypes, ArgTypes).

etc_(Expr, Type, Cont) :-
    Expr =.. [Func|Args],
    etc_map_(Args, ArgTypes, Cont),
    (   registered_functions(Func, _, Type, ArgTypes, Cont)
    ->  !, true
    ;   registered_functions(Func, FuncName, Type, FuncArgTypes, Cont),
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
    write("function '"), write(FuncId), write("' not declared"), nl, fail.


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
    write("couldnt match type for function: "),
    write(F), nl,
    fail.


args_match(FuncName, As, Bs) :-
    (   same_length(As, Bs)
    ->  args_match_(As, Bs, 1, FuncName)
    ;   length(As, LA), length(Bs, LB),
        write("function '"), write(FuncName), write("' expects "),
        write(LA), write(" arguments but got "),
        write(LB), nl,
        fail
    ).

args_match_([], [], _N, _FuncName).
args_match_([A|As], [B|Bs], N, FuncName) :-
    N1 is N + 1,
    (   A = B
    ->  !, args_match_(As, Bs, N1, FuncName)
    ;   !,
        write("argument "), write(N),
        write(" of "), write(FuncName),
        write(" should be of type "), write(A),
        write(" but got "), write(B), nl, fail
    ).



% === CONTEXTS ===
% context is a list variable-type mappings
% head of context is the mapping of the current block
% a variable type mapping is represented by [(name, type)]

% subcontext(+Context, -SubContext)
context_sub(Context, [[]|Context]).

% context_insert(+Context, +Name, +Type, -NewContext)
context_insert([Curr|Outers], Name, Type, [NewCurr|Outers]) :-
    map_insert(Curr, Name, Type, NewCurr).

% get_type_context(+Context, +Name, -Type)
context_get_type(Context, Name, Type) :-
    member(Map, Context),
    map_get_type(Map, Name, TypeFound), !,
    (   Type = TypeFound
    ->  true
    ;   write("wrong type: '"), write(Name),
        write("' has type '"), write(TypeFound),
        write("' but was assigned type '"), write(Type),
        write("'"), nl, fail
    ).

context_get_type(_, _, _) :-
    write("variable not declared"), nl, fail.


% map_insert(+Map, +Name, +Type, -NewMap)
map_insert(Map, Name, _, _) :-
    map_get_type(Map, Name, _), !,
    write("redeclaration of variable"), nl, fail.

map_insert(Map, Name, Type, [(Name, Type)|Map]).

% get_type_map(+Map, +Name, -Type)
map_get_type([(Name, Type)|_], Name, Type) :- !.
map_get_type([_|Map], Name, Type) :- map_get_type(Map, Name, Type).



% === UTILS ===

same_length([], []).
same_length([_|L1], [_|L2]) :- same_length(L1, L2).
