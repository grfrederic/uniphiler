:- use_module('parse.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse_and_print(SourceFile),
    halt(0).

main(_) :-
    write("Usage:swipl -q -t halt show_parse.prolog [source file]"), nl,
    halt(1).


parse_and_print(SourceFile) :-
    parse(SourceFile, AST),
    write(AST), nl.

parse_and_print(_) :-
    write("Parsing failed."), nl.
