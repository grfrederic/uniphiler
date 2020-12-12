:- use_module('parse.prolog').
:- use_module('checks.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse_and_check(SourceFile),
    halt(0).

main(_) :-
    write("Usage:swipl -q -t halt show_ast.prolog [source file]"), nl,
    halt(1).


parse_and_check(SourceFile) :-
    parse(SourceFile, AST),
    all_checks(AST),
    write("OK"), nl.

parse_and_check(_) :-
    write("some error occured during analysis"), nl,
    halt(1).
