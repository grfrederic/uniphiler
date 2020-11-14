:- use_module('parse.prolog').
:- use_module('jvm.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse(SourceFile, AST),
    compile_to_jvm(SourceFile, AST, JVM),
    write(JVM),
    halt(0).

main(_) :-
    write("Usage:swipl -q -t halt compile_jvm.prolog [source file]"), nl,
    halt(1).
