:- module(context, [
    context_sub/2,
    context_update/4,
    context_insert/4,
    context_get/3,
    map_update/4,
    map_insert/4,
    map_get/3
]).

:- use_module('errors.prolog').


% === CONTEXTS ===
% context is a list variable-stuff mappings
% head of context is the mapping of the current block
% a variable-stuff mapping is represented by [(name, stuff)]


% subcontext(+Context, -SubContext)
context_sub(Context, [[]|Context]).

% set first occurance of Name to Val, fail on not found
% context_update(+Context, +Name, +Val, -NewContext)
context_update([Curr|Outers], Name, Val, [NewCurr|Outers]) :-
    map_update(Curr, Name, Val, NewCurr), !.

context_update([Curr|Outers], Name, Val, [Curr|NewOuters]) :-
    context_update(Outers, Name, Val, NewOuters), !.

context_update(_, Name, _, _) :-
    error(["tried to update", Name, "but could not find it"]).


% insert Name -> Val into current context, error on collision
% context_insert(+Context, +Name, +Val, -NewContext)
context_insert([Curr|Outers], Name, Val, [NewCurr|Outers]) :-
    map_insert(Curr, Name, Val, NewCurr).

% context_get(+Context, +Name, -Val)
context_get(Context, Name, Val) :-
    member(Map, Context),
    map_get(Map, Name, ValFound), !,
    ( Val = ValFound, !
    ; error([Name, "is", ValFound, "but expected", Val])
    ).

context_get(_, _, _) :-
    error("variable not declared").


% map_update(+Map, +Name, +Val, -NewMap)
map_update([(Name, _Val)|Rest], Name, Val, [(Name, Val)|Rest]) :- !.

map_update([H|Map], Name, Val, [H|NewMap]) :-
    map_update(Map, Name, Val, NewMap).


% map_insert(+Map, +Name, +Val, -NewMap)
map_insert(Map, Name, _, _) :-
    map_get(Map, Name, _), !,
    error("redeclaration of variable").

map_insert(Map, Name, Val, [(Name, Val)|Map]).

% map_get(+Map, +Name, -Val)
map_get([(Name, Val)|_], Name, Val) :- !.
map_get([_|Map], Name, Val) :- map_get(Map, Name, Val).
