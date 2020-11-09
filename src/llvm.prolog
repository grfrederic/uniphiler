:- module(llvm, [compile_to_llvm/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).


compile_to_llvm(AST, LLVM) :-
    phrase(program_llvm(AST), LLVM_),
    string_codes(LLVM, LLVM_).


program_llvm(AST) --> program_start, main(AST).


program_start --> format_string_nr, format_string_nl, declare_printf, newline.

format_string_nr --> "@formatStringNr = private constant [3 x i8] c\"%d\\00\"", newline.
format_string_nl --> "@formatStringNl = private constant [2 x i8] c\"\\0A\\00\"", newline.
declare_printf --> "declare i32 @printf(i8*, ...)", newline.


main(AST) --> sequence(main_start, stmt, "", main_end, AST).
main_start --> "define i32 @main() {", newline.
main_end --> return, "}".


stmt(sass(I, E)) --> {id_to_reg(I, R)}, exp(E, R), newline.
stmt(sexp(E)) --> exp(E, O), print_nr_call(O, _), print_nl_call(_).

exp(exp_var(I), O) --> {id_to_reg(I, O)}.
exp(exp_lit(N), O) --> {nr_to_res(N, O)}.


exp(E, O) -->
    {compound_name_arguments(E, F, Args)},
    prepare_args(Args, Outs),
    indent, reg(O), " = ", fun(F), " i32 ", regs(Outs), newline.

prepare_args([A|As], [O|Os]) --> exp(A, O), prepare_args(As, Os).
prepare_args([], []) --> [].


% === SPECIAL CALLS
return --> indent, "ret i32 0", newline.

print_nr_call(RI, RO) --> indent, reg(RO), " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @formatStringNr, i64 0, i64 0), i32 ", reg(RI), ")", newline.
print_nl_call(RO) --> indent, reg(RO), " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @formatStringNl, i64 0, i64 0))", newline.



% === PRINTING UTILS ===

fun(exp_add) --> !, "add".
fun(exp_sub) --> !, "sub".
fun(exp_mul) --> !, "mul".
fun(exp_div) --> !, "div".
fun(F) --> {term_string(F, FN)}, !, FN.

reg(O) --> {nonvar(O)}, !, O.
reg(O) --> {var(O), term_string(O, S), string_concat("%", S, O)}, !, O.

regs(Os) --> sequence(reg, ", ", Os).

id_to_reg(token(id(I), _Loc), R) :- string_concat("%var_", I, R).
nr_to_res(token(nr(N), _Loc), R) :- term_string(N, R).

indent --> "  ".
newline --> "\n".
