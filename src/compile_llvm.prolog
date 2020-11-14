:- use_module('parse.prolog').
:- use_module('llvm.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse(SourceFile, AST),
    compile_to_llvm(AST, LLVM),
    write(LLVM),
    halt(0).

main(_) :-
    write("Usage:swipl -q -t halt compile_llvm.prolog [source file]"), nl,
    halt(1).
