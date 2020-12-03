:- use_module('parse.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse(SourceFile, AST),
    write(AST), nl,
    halt(0).

main(_) :-
    write("Usage:swipl -q -t halt show_ast.prolog [source file]"), nl,
    halt(1).
