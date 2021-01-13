:- module(tokenize, [tokenize/2, tokens/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pure_input)).

:- use_module('errors.prolog').


tokenize(File, Tokens) :-
    phrase_from_file(tokens(Tokens), File), !.


tokens(Tokens) -->
    sequence(maybe_token, MaybeTokens),
    {drop_nones(MaybeTokens, Tokens)}.

maybe_token(token(M, L)) --> maybe_token_(M), !, lazy_list_location(L).

% comments
maybe_token_(none) --> "#", !, string_without("\n", _RestOfLine).
maybe_token_(none) --> "//", !, string_without("\n", _RestOfLine).
maybe_token_(none) -->
    "/*", !, 
    (   (string(_Comment), "*/")
    ->  []
    ;   lazy_list_location(L),
        { error(["started comment but never finished"], L) }
    ).

% whitespace
maybe_token_(none) --> blank.

% ops
maybe_token_(operator("||")) --> "||".
maybe_token_(operator("&&")) --> "&&".
maybe_token_(operator("--")) --> "--".
maybe_token_(operator("++")) --> "++".
maybe_token_(operator("<=")) --> "<=".
maybe_token_(operator(">=")) --> ">=".
maybe_token_(operator("==")) --> "==".
maybe_token_(operator("!=")) --> "!=".
maybe_token_(operator("-")) --> "-".
maybe_token_(operator("+")) --> "+".
maybe_token_(operator("*")) --> "*".
maybe_token_(operator("/")) --> "/".
maybe_token_(operator("%")) --> "%".
maybe_token_(operator(">")) --> ">".
maybe_token_(operator("<")) --> "<".
maybe_token_(operator("=")) --> "=".
maybe_token_(operator("!")) --> "!".

% struct
maybe_token_(s("(")) --> "(".
maybe_token_(s(")")) --> ")".
maybe_token_(s("{")) --> "{".
maybe_token_(s("}")) --> "}".
maybe_token_(s(";")) --> ";".
maybe_token_(s(",")) --> ",".

% int and str literals
maybe_token_(lit_int(N)) --> clot([C|Cs], digit), !, {number_codes(N, [C|Cs])}.
maybe_token_(lit_str(S)) --> "\"", string(Cs), "\"", {string_codes(S, Cs)}.

% syntax and vars
maybe_token_(T) --> clot([C|Cs], csym), {char_type(C, csymf), string_codes(S, [C|Cs]), string_token(S, T)}.

% stop and error
maybe_token_(_) --> eos, !, {fail}.
maybe_token_(_) --> { error("failed to parse token") }.


% dispatch strings
string_token("return", return).
string_token("if", if).
string_token("else", else).
string_token("while", while).

string_token("void", type(void)).
string_token("int", type(int)).
string_token("string", type(str)).
string_token("boolean", type(boolean)).

string_token("true", lit_bool(true)).
string_token("false", lit_bool(false)).

string_token(S, id(S)).


% utils
drop_nones([token(none, _)|Ms], Ts) :- !, drop_nones(Ms, Ts).
drop_nones([H|Ms], [H|Ts]) :- drop_nones(Ms, Ts).
drop_nones([], []).

% char list of type
clot([C|Cs], Type) --> [C], {char_type(C, Type)}, !, clot(Cs, Type).
clot([], _) --> [].
