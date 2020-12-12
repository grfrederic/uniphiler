:- use_module('tokenize.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    tokenize(SourceFile, Tokens),
    write(Tokens), nl,
    halt(0).

main(_) :-
    write("Usage:swipl -q -t halt show_tokens.prolog [source file]"), nl,
    halt(1).
