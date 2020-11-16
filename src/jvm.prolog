:- module(jvm, [compile_to_jvm/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).


compile_to_jvm(SourceFile, AST, JVM) :-
    phrase(program_jvm(SourceFile, AST), JVM_),
    string_codes(JVM, JVM_),
    !. % compilation is deterministic

compile_to_jvm(_, _, _) :-
    throw(compilation_failed).


program_jvm(SourceFile, AST) -->
    metadata(SourceFile),
    class(AST).

metadata(SourceFile) -->
    {file_base_name(SourceFile, Name),
     split_string(Name, ".", "", [ClassName|_]),
     term_string(SourceFile, Source)},
    ".bytecode 58.0", newline,
    ".source ", Source, newline,
    ".class ", ClassName, newline.

class(AST) -->
    ".super java/lang/Object", newline,
    newline,
    method_init,
    newline,
    method_main(AST).

method_init -->
    ".method <init>()V", newline,
    indent, ".limit stack 1", newline,
    indent, ".limit locals 1", newline,
    indent, "aload_0", newline,
    indent, "invokespecial java/lang/Object/<init>()V", newline,
    indent, "return", newline,
    ".end method", newline.

% we use phrase because we need to generate _stmts_ first,
% so that we know what stack size we should declare
method_main(AST) -->
    {vars(AST, Vars),
     phrase(stmts(AST, Vars, Stack), StmtsCode)},  % calculates Stack
    ".method public static main([Ljava/lang/String;)V", newline,
    limit_stack(Stack),
    limit_locals(Vars),
    StmtsCode,
    indent, "return", newline,
    ".end method", newline.

limit_stack(Stack) --> indent, ".limit stack ", number(Stack), newline.
limit_locals(Vars) --> {length(Vars, N), N1 is N + 1}, indent, ".limit locals ", number(N1), newline.


% stmts(+Stmts, +Vars, -Stack)
stmts(Stmts, Vars, Stack) --> stmts_(Stmts, Vars, Ss), {max_member(Stack, Ss)}.

stmts_([], _, []) --> [].
stmts_([Stmt|Stmts], Vars, [Stack|Stacks]) -->
    stmt(Stmt, Vars, Stack),
    newline,
    stmts_(Stmts, Vars, Stacks).


stmt(sass(I, E), Vars, Stack) -->
    {get_var_id(I, Vars, Id)},
    exp(E, Vars, Stack),
    store_int(Id).

stmt(sexp(E), Vars, StackStmt) -->
    push_io_out,
    exp(E, Vars, StackExp),
    call_println,
    {StackStmt is StackExp + 1}.


% exp(+Exp, +Vars, -Stack)
% Vars - list of Vars in current context
% Stack - stack required for evaluating Expr
% result is left on stack
exp(exp_var(I), Vars, 1) --> !, {get_var_id(I, Vars, Id)}, push_var(Id).
exp(exp_lit(token(nr(N), _)), _, 1) --> !, push_int(N).

% optimize stack for binary operations
exp(E, Vars, S) -->
    {compound_name_arguments(E, BinOp, [E1, E2])}, !,
    {phrase(exp(E1, Vars, S1), C1),
     phrase(exp(E2, Vars, S2), C2),
     (S1 > S2 -> ChgOrd = false, ArgCode = (C1, C2), S is max(S1, S2 + 1);
                 ChgOrd = true, ArgCode = (C2, C1), S is max(S2, S1 + 1))},
    ArgCode, 
    maybe_swap(ChgOrd, BinOp),
    indent, fun(BinOp), newline.

exp(E, Vars, S) -->
    {compound_name_arguments(E, F, Args)},
    prepare_args(Args, Vars, Ss),
    indent, fun(F), newline,
    {total_stack(Ss, S)}.

prepare_args([], _, []) --> [].
prepare_args([A|As], Vars, [S|Ss]) --> exp(A, Vars, S), prepare_args(As, Vars, Ss).

maybe_swap(true, BinOp) --> {\+ commutative(BinOp)}, !, swap.
maybe_swap(_, _) --> [].

total_stack(Ss, T1) :-
    total_stack_(Ss, T, 0), T1 is max(T, 1).  % at least 1 for return value

total_stack_([], -1, _).
total_stack_([S|Ss], T, N) :-
    SN is S + N,
    N1 is N + 1,
    total_stack_(Ss, R, N1),
    T is max(SN, R).


% emitting

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


% === COMMUTATIVE ===

commutative(exp_add).
commutative(exp_mul).


% === SPECIAL CALLS ===

swap --> indent, "swap", newline.

push_io_out --> indent, "getstatic java/lang/System/out Ljava/io/PrintStream;", newline.
call_println --> indent, "invokevirtual java/io/PrintStream/println(I)V", newline.


% === GET LIST OF VARS USED ===
%
% We need to number all variables used in a method. Prolog doesn't really
% have a good structure for this, so we just keep them on a list and use
% their positions as Ids.

% vars(+AST, -Vars)
% get list of all variables used
vars(AST, Vars) :- setof(I, L^E^member(sass(token(id(I), L), E), AST), Vars).
vars(_, []).

% get_var_id(+Token, +Vars, ?Id)
% check Id of Token variable (by looking up its position)
get_var_id(token(id(I), _), Vars, Id) :- nth1(Id, Vars, I), !.
get_var_id(_, _, _) :- throw(reference_before_assignment).


indent --> "  ".
newline --> "\n".
