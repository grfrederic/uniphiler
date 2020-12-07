:- module(parse, [parse/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module('tokenize.prolog').


parse(File, AST) :-
    tokenize(File, Tokens),
    phrase(program(AST), Tokens), !. % parser is deterministic

parse(_, _) :-
    throw(parser_failed).


program(AST) --> sequence(stmt, t(semicolon), AST).


stmt(sass(I, E)) --> id(I), t(equals_sign), exp(E).
stmt(sexp(E)) --> exp(E).


% === EXPRESSIONS ===

exp(E) --> exp1(E).

exp1(E) --> build(right, exp2, t(plus_sign), exp_add, E).
exp2(E) --> build(left, exp3, t(minus_sign), exp_sub, E).
exp3(E) --> build(left, exp4, t(asterisk), exp_mul, E).
exp4(E) --> build(left, exp5, t(slash), exp_div, E).

% just literals and variables
exp5(exp_lit(I)) --> nr(I), !.
exp5(exp_var(I)) --> id(I), !.
exp5(E) --> t(left_parenthesis), exp(E), t(right_parenthesis).


% === BUILD ASSOC ===

build(left, El, Sep, Op, Res) --> sequence(El, Sep, Els), {laf(Op, Els, Res)}.
build(right, El, Sep, Op, Res) --> sequence(El, Sep, Els), {raf(Op, Els, Res)}.

laf(Op, [H|T], R) :- laf_(Op, H, T, R).
laf_(_, A, [], A).
laf_(Op, A, [H|T], R) :- NA =.. [Op, A, H], laf_(Op, NA, T, R).

raf(_, [E], E).
raf(Op, [E1, E2], R) :- R =.. [Op, E1, E2].
raf(Op, [H|T], R) :- raf(Op, T, RT), R =.. [Op, H, RT].


% === TOKEN UTILS ===

t(TokenType) --> [token(TokenType, _)].
id(token(id(I), Loc)) --> [token(id(I), Loc)].
nr(token(nr(I), Loc)) --> [token(nr(I), Loc)].
