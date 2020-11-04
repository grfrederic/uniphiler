:- use_module('parse.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse(SourceFile, AST),
    write(AST), nl,
    halt(0).

main(_) :-
    write("Usage: compile.prolog [source file]"), nl,
    halt(1).
