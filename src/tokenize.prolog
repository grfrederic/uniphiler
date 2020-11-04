:- module(tokenize, [tokenize/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pure_input)).


tokenize(File, Tokens) :-
    phrase_from_file(tokens(Tokens), File).

tokens(Tokens) --> blanks, tokens_(Tokens).

tokens_([T|Ts]) --> token(T), !, blanks, tokens_(Ts).
tokens_([]) --> blanks, eos.

% token holds the raw token and its lazy loc (in case we need it for error msg)
token(token(T, L)) --> token_raw(T), lazy_list_location(L).

% TODO(frederic): number will read +3 as [3] and not [+, 3]
token_raw(nr(T)) --> number(T), !.
token_raw(id(T)) --> clot([C|Cs], csym), !, {string_codes(T, [C|Cs])}.
token_raw(semicolon) --> ";", !.
token_raw(left_parenthesis) --> "(", !.
token_raw(right_parenthesis) --> ")", !.
token_raw(equals_sign) --> "=", !.
token_raw(plus_sign) --> "+", !.
token_raw(minus_sign) --> "-", !.
token_raw(asterisk) --> "*", !.
token_raw(slash) --> "/", !.
token_raw(_) --> syntax_error("Tokenizer failed: unexpected symbol.").

% TODO(frederic): debug
%token_raw(sp(T)) --> [C], !, {string_codes(T, [C])}.

% char list of type
clot([C|Cs], Type) --> [C], {char_type(C, Type)}, !, clot(Cs, Type).
clot([], _) --> [].
