:- module(parse, [parse/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module('tokenize.prolog').


parse(File, AST) :-
    tokenize(File, Tokens),
    phrase(program(AST), Tokens).


program(AST) --> sequence(stmt, t(semicolon), AST).


stmt(sass(I, E)) --> id(I), t(equals_sign), exp(E).
stmt(sexp(E)) --> exp(E).


% === EXPRESSIONS ===

exp(E) --> exp1(E).

% addition is right assoc
exp1(exp_add(E1, E2)) --> exp2(E1), t(plus_sign), exp1(E2).
exp1(T) --> exp2(T).


% subtraction, factor out tail for left assoc
exp2(E) --> exp3(H), exp2tail(H, E).
exp2(E) --> exp3(E).

exp2tail(A, A) --> [].
exp2tail(A, E) --> t(minus_sign), exp3(S), exp2tail(exp_sub(A, S), E).


% mul/div also factor out tail for left assoc
exp3(E) --> exp4(H), exp3tail(H, E).
exp3(T) --> exp4(T).

exp3tail(A, A) --> [].
exp3tail(A, E) --> t(asterisk), exp4(S), exp3tail(exp_mul(A, S), E).
exp3tail(A, E) --> t(slash), exp4(S), exp3tail(exp_div(A, S), E).


% just literals and variables
exp4(exp_lit(I)) --> nr(I).
exp4(exp_var(I)) --> id(I).


% === TOKEN UTILS ===

t(TokenType) --> [token(TokenType, _)].
id(token(id(I), Loc)) --> [token(id(I), Loc)].
nr(token(nr(I), Loc)) --> [token(nr(I), Loc)].
