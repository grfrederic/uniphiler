:- module(errors, [
    error/1,
    error/2,
    complain_on_fail/1,
    complain_on_fail/2,
    error_stack_print/0,
    error_stack_clear/0,
    error_stack_push/1
]).


% === ERROR MSGS ===

error(Msg) :- error_stack_push(Msg), fail.

error(Msg, Loc) :-
    stringify(Msg, MsgStr),
    error_stack_push([MsgStr, "at", Loc]),
    fail.

complain_on_fail(_).
complain_on_fail(Msg) :- error(Msg).

complain_on_fail(_, _).
complain_on_fail(Msg, Loc) :- error(Msg, Loc).


:- dynamic('error_stack'/1).

error_stack_clear :-
    retractall(error_stack(_)).


error_stack_push(Msg) :-
    stringify(Msg, MsgStr),
    assertz(error_stack(MsgStr)).


error_stack_print :-
    writeln("ERROR"),
    error_stack_print_.

error_stack_print_ :-
    error_stack(Msg), writeln(Msg), fail.

error_stack_print_.


% === PRINTING ===

stringify(X, S) :-
    is_list(X), !,
    maplist(pretty_term_string, X, Ss),
    atomics_to_string(Ss, " ", S).

stringify(X, S) :-
    pretty_term_string(X, S).


pretty_term_string(X, S) :- string(X), !, S = X.
pretty_term_string(X, S) :- number(X), !, number_string(X, S).
pretty_term_string(X, S) :- X =.. [file|_], !, term_string(X, S).
pretty_term_string(X, S) :- 
    term_string(X, T),
    atomics_to_string(["'", T, "'"], S).
