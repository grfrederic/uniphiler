:- use_module('parse.prolog').
:- use_module('jvm.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse(SourceFile, AST),
    compile_to_jvm(AST, JVM),
    write(JVM),
    halt(0).

main(_) :-
    write("Usage: compile.prolog [source file]"), nl,
    halt(1).
