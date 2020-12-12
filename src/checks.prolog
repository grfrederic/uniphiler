:- module(checks, [
    check_all_args_different/1,
    context_new/1,
    context_sub/2,
    context_insert/4,
    context_get_type/3,
    context_type_expr/3
]).


% === ARGS DIFFERENT ===

check_all_args_different([]) :- !.
check_all_args_different(Args) :-
    bagof(ArgName, ArgType^member((ArgName, ArgType), Args), ArgNamesBag),
    sort(ArgNamesBag, ArgNamesSet),
    same_length(ArgNamesSet, ArgNamesBag), !.

check_all_args_different(_) :-
    write("argument names have duplicates"), nl, fail.


% === TYPING ===
context_type_expr(Cont, Type, Expr) :- etc_(Expr, Type, Cont).


% for these don't check args
etc_(expr_id(I), Type, Cont) :- !, context_get_type(Cont, I, Type).
etc_(expr_str(_), str, _) :- !.
etc_(expr_int(_), int, _) :- !.
etc_(expr_bool(_), boolean, _) :- !.

etc_(expr_ap(FuncId, Args), Type, Cont) :- !,
    context_get_func_type(FuncId, Type, FuncArgTypes),
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


context_get_func_type("printInt", void, [int]) :- !.
context_get_func_type("printString", void, [str]) :- !.
context_get_func_type(_FuncId, _Type, _FuncArgTypes) :-
    write("warning: function types not checked"), nl.


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
    args_match_(As, Bs, 1, FuncName).

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

% new_context(-Context)
context_new([]).

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
