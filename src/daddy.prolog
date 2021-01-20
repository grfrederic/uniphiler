:- module(daddy, [compute_daddies/2]).


compute_daddies(Graph, AllDaddies) :-
    init_daddies(Graph, AllDs0),
    daddies_loop(Graph, AllDs0, AllDaddies).


daddies_loop(Graph, AllDs, AllDsOut) :-
    daddies_step(Graph, AllDs, AllDsNext), !,
    % writeln("------------"),
    % maplist(writeln, AllDs),
    (   AllDs == AllDsNext
    ->  AllDsOut = AllDsNext, !
    ;   daddies_loop(Graph, AllDsNext, AllDsOut)
    ).


daddies_step(Graph, AllDs, AllDsNext) :-
    maplist(daddies_step_row(AllDs), Graph, AllDsNext).

daddies_step_row(AllDs, (Label, Ins), (Label, DsNext)) :-
    maplist(get_ds(AllDs), Ins, InDs),
    inter_hard(InDs, DsNextRest),
    add_hard(Label, DsNextRest, DsNext).


init_daddies(Graph, AllDs) :-
    nodes(Graph, Nodes),
    Nodes = [Entry|Rest],
    init_daddies_rest(Rest, Nodes, RestDs),
    AllDs = [(Entry, [Entry])|RestDs]. 

init_daddies_rest(Rest, Nodes, RestDs) :-
    maplist(idr_(Nodes), Rest, RestDs).

idr_(Nodes, Label, (Label, Nodes)).


get_ds([(L1, Ds)|_], L2, Ds) :- L1 == L2, !.
get_ds([_|AllDs], L, Ds) :- get_ds(AllDs, L, Ds).


add_hard(X, L, L) :- member_hard(X, L), !.
add_hard(X, L, [X|L]) :- !.


inter_hard([], []) :- !.
inter_hard([V], V) :- !.
inter_hard([V|Vs], I) :- foldl(is2_, Vs, V, I).

is2_([], _, []).
is2_([H|T], V, M) :-
    (   member_hard(H, V)
    ->  M = [H|M1], is2_(T, V, M1)
    ;   is2_(T, V, M)
    ).


% hard_member(+X, +L).
member_hard(X, [Y|_]) :- X == Y, !.
member_hard(X, [_|L]) :- member_hard(X, L).


nodes(Graph, Nodes) :-
    maplist(get_label, Graph, Nodes).

get_label((Label, _), Label).


% === example graph ===

% [(Node, Ins)]
%graph1([
%    (E, []),
%    (A, [E]),
%    (B, [A, C]),
%    (C, [A, D]),
%    (D, [B]),
%    (_Final, [D])
%]).
