:- module(jvm, [compile_to_jvm/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).


compile_to_jvm(SourceFile, AST, JVM) :-
    phrase(program_jvm(SourceFile, AST), JVM_),
    string_codes(JVM, JVM_).


program_jvm(SourceFile, AST) -->
    {name_class(SourceFile, ClassName),
     term_string(SourceFile, Source)},
    ".bytecode 58.0", newline,
    ".source ", Source, newline,
    ".class ", ClassName, newline,
    ".super java/lang/Object", newline,
    newline,
    ".method <init>()V", newline,
    indent, ".limit stack 1", newline,
    indent, ".limit locals 1", newline,
    indent, "aload_0", newline,
    indent, "invokespecial java/lang/Object/<init>()V", newline,
    indent, "return", newline,
    ".end method", newline,
    newline,
    main(AST).


name_class(SourceFile, ClassName) :-
    file_base_name(SourceFile, Name),
    split_string(Name, ".", "", [ClassName|_]).


main(AST) --> {vars(AST, Vars)},
              ".method public static main([Ljava/lang/String;)V", newline,
              limit_stack(AST),
              limit_locals(Vars),
              stmts(AST, Vars), 
              indent, "return", newline,
              ".end method", newline.


stmts([S|Stmts], Vars) --> stmt(S, Vars), !, newline, stmts(Stmts, Vars).
stmts([], _) --> [].

stmt(sass(I, E), Vars) --> {get_var_id(I, Vars, Id)}, exp(E, Vars), store_int(Id).
stmt(sexp(E), Vars) --> push_io_out, exp(E, Vars), call_println.


exp(exp_var(I), Vars) --> !, {get_var_id(I, Vars, Id)}, push_var(Id).

exp(exp_lit(token(nr(N), _)), _) --> !, push_int(N).


exp(E, Vars) --> {compound_name_arguments(E, F, Args)},
                 prepare_args(Args, Vars),
                 indent, fun(F), newline.

prepare_args([A|As], Vars) --> exp(A, Vars), prepare_args(As, Vars).
prepare_args([], _) --> [].


fun(exp_add) --> !, "iadd".
fun(exp_sub) --> !, "isub".
fun(exp_mul) --> !, "imul".
fun(exp_div) --> !, "idiv".
fun(F) --> {term_string(F, FN)}, !, FN.


push_int(0) --> !, indent, "iconst_0", newline.
push_int(1) --> !, indent, "iconst_1", newline.
push_int(2) --> !, indent, "iconst_2", newline.
push_int(3) --> !, indent, "iconst_3", newline.
push_int(4) --> !, indent, "iconst_4", newline.
push_int(5) --> !, indent, "iconst_5", newline.
push_int(-1) --> !, indent, "iconst_m1", newline.
push_int(N) --> {-128 =< N, N =< 127}, !, indent, "bipush ", number(N), newline.
push_int(N) --> {-32768 =< N, N =< 32767}, !, indent, "sipush ", number(N), newline.
push_int(N) --> indent, "ldc ", number(N), newline.

store_int(0) --> indent, "istore_0", newline.
store_int(1) --> indent, "istore_1", newline.
store_int(2) --> indent, "istore_2", newline.
store_int(3) --> indent, "istore_3", newline.
store_int(N) --> indent, "istore ", number(N), newline.

push_var(0) --> !, indent, "iload_0", newline.
push_var(1) --> !, indent, "iload_1", newline.
push_var(2) --> !, indent, "iload_2", newline.
push_var(3) --> !, indent, "iload_3", newline.
push_var(I) --> indent, "iload ", number(I), newline.

push_io_out --> indent, "getstatic java/lang/System/out Ljava/io/PrintStream;", newline.
call_println --> indent, "invokevirtual java/io/PrintStream/println(I)V", newline.


% === GET LIST OF VARS USED ===
vars(AST, Vars) :- setof(I, L^E^member(sass(token(id(I), L), E), AST), Vars).
vars(_, []).

get_var_id(token(id(I), _), Vars, Id) :- nth1(Id, Vars, I).


% === COMPUTE STACK LIMIT ===
limit_stack(Stmts) --> {maplist(stmt_stack, Stmts, Sizes), max_member(S, Sizes)},
                       indent, ".limit stack ", number(S), newline.

stmt_stack(sass(_, E), S) :- exp_stack(E, S).
stmt_stack(sexp(E), S1) :- exp_stack(E, S), S1 is S + 1.

exp_stack(exp_lit(_), 1) :- !.
exp_stack(exp_var(_), 1) :- !.
exp_stack(E, S) :- compound_name_arguments(E, _, Args), exp_stack_(Args, S).

exp_stack_(Args, S) :- exp_stack_args(Args, Ss, 0), max_member(S, Ss).

exp_stack_args([A|Args], [S|Ss], N) :-
    exp_stack(A, S1),
    S is S1 + N,
    N1 is N + 1,
    exp_stack_args(Args, Ss, N1).

exp_stack_args([], [], _).


% === COMPUTE LOCALS LIMIT ===
limit_locals(Vars) --> {length(Vars, N), N1 is N + 1}, indent, ".limit locals ", number(N1), newline.


indent --> "  ".
newline --> "\n".
