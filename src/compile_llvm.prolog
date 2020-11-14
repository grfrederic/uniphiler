:- use_module('parse.prolog').
:- use_module('llvm.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse(SourceFile, AST),
    compile_to_llvm(AST, LLVM),
    write(LLVM),
    halt(0).

main(_) :-
    write("Usage: compile.prolog [source file]"), nl,
    halt(1).
