:- module(tokenize, [tokenize/2, tokens/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).


tokenize(File, Tokens) :-
    phrase_from_file(tokens(Tokens), File).


% tokens may be separated by blanks
tokens(Tokens) --> iws, tokens_(Tokens).

tokens_([]) --> iws, eos.
tokens_([T|Ts]) --> token(T), iws, tokens_(Ts).


% token holds the raw token and its lazy loc (in case we need it for error msg)
token(token(T, L)) --> token_raw(T), lazy_list_location(L).

% numbers are NOT allowed to have +/- in front of them
token_raw(nr(N)) --> clot([C|Cs], digit), !, {number_codes(N, [C|Cs])}.
token_raw(id(T)) --> clot([C|Cs], csym), !, {string_codes(T, [C|Cs])}.
token_raw(plus_sign) --> "+", !.
token_raw(minus_sign) --> "-", !.
token_raw(semicolon) --> ";", !.
token_raw(left_parenthesis) --> "(", !.
token_raw(right_parenthesis) --> ")", !.
token_raw(equals_sign) --> "=", !.
token_raw(asterisk) --> "*", !.
token_raw(slash) --> "/", !.
token_raw(_) --> syntax_error("Tokenizer failed: unexpected symbol.").

% ignore whitespace
iws --> clot(_, space).

% char list of type
clot([C|Cs], Type) --> [C], {char_type(C, Type)}, !, clot(Cs, Type).
clot([], _) --> [].
