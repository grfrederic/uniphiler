:- module(errors, [
    complain_at_loc/2,
    complain_on_fail/2
]).


% === ERROR MSGS ===

complain_at_loc(S, L) :- write(S), write(" at "), write(L), nl.

complain_on_fail(_, _).
complain_on_fail(S, L) :- complain_at_loc(S, L), fail.
