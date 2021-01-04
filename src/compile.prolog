:- use_module('parse.prolog').
:- use_module('typing_and_checks.prolog').
:- use_module('llvm.prolog').


:- initialization (main, main).


main([SourceFile|_]) :-
    parse_and_check(SourceFile),
    halt(0).

main(_) :-
    write("Usage:swipl -q -t halt compile.prolog [source file]"), nl,
    halt(1).


parse_and_check(SourceFile) :-
    parse(SourceFile, AST),
    check_and_derive_types(AST),
    compile_to_llvm(AST, LLVM),
    writeln(LLVM).

parse_and_check(_) :-
    halt(1).
