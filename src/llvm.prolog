:- module(llvm, [compile_to_llvm/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).


compile_to_llvm(AST, LLVM) :-
    phrase(program_llvm(AST), LLVM_),
    string_codes(LLVM, LLVM_),
    !. % compilation is deterministic

compile_to_llvm(_, _) :-
    throw(compilation_failed).


program_llvm(AST) --> printing_header,
                      newline,
                      function("main", AST).

printing_header -->
    "@formatStringNr = private constant [3 x i8] c\"%d\\00\"", newline,
    "@formatStringNl = private constant [2 x i8] c\"\\0A\\00\"", newline,
    "declare i32 @printf(i8*, ...)", newline.

function(Name, AST) -->
    {reset_reg_iter},
    sequence(("define i32 @", Name, "() {", newline),  % start
             stmt,                                     % elem
             "",                                       % separator
             (return, "}", newline),                   % end
             AST).


stmt(sass(I, E)) --> exp(E, R), {set_id_val(I, R)}.
stmt(sexp(E)) --> exp(E, O), print_nr_call(O, _), print_nl_call(_).


% exp(Expression, Result)
% Result can be used as argument to function calls, e.g. a register or literal
exp(exp_var(I), O) --> !, {get_id_val(I, O)}.
exp(exp_lit(N), O) --> !, {nr_to_res(N, O)}.

exp(E, O) --> {compound_name_arguments(E, F, Args)},
              prepare_args(Args, Outs),
              indent, reg(O), " = ", fun(F), " i32 ", regs(Outs), newline.

prepare_args([A|As], [O|Os]) --> exp(A, O), prepare_args(As, Os).
prepare_args([], []) --> [].

% emitting

fun(exp_add) --> !, "add".
fun(exp_sub) --> !, "sub".
fun(exp_mul) --> !, "mul".
fun(exp_div) --> !, "sdiv".
fun(F) --> {term_string(F, FN)}, FN.


% === SPECIAL CALLS ===

return --> indent, "ret i32 0", newline.

print_nr_call(RI, RO) --> indent, reg(RO), " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @formatStringNr, i64 0, i64 0), i32 ", reg(RI), ")", newline.
print_nl_call(RO) --> indent, reg(RO), " = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @formatStringNl, i64 0, i64 0))", newline.


% === HANDLING OF VARIABLE ITERATIONS ===

% We're dealing with SSA, so we're not allowed to assign values to a register.
% Because of that, when compiling to LLVM, it is necessary to substitute
% subsequent uses of the same variable with the value/location of the value
% assigned to it. This dynamic predicate stores this information during function
% generation.

:- dynamic('id_val_'/2).

% set_id_val(+I, +V)
set_id_val(I, V) :-
    unpack_id(I, J),
    retractall(id_val_(J, _)),
    asserta(id_val_(J, V)).

% get_id_val(+I, ?V)
get_id_val(I, V) :- unpack_id(I, J), id_val_(J, V).
get_id_val(_, _) :- throw(reference_before_assignment).

unpack_id(token(id(I), _Loc), I).


% We will also need a supply of fresh register names. We handle this
% with a dynamic predicate keeping track of the current variable iteration
% during function generation.
%
:- dynamic('reg_iter_'/1).

% reset generation counter to 1
reset_reg_iter :-
    retractall(reg_iter_(_)),
    asserta(reg_iter_(1)).

% get fresh iteration and inc counter
get_reg_iter(N) :-
    reg_iter_(N),
    retractall(reg_iter_(_)),
    N1 is N + 1,
    asserta(reg_iter_(N1)).

% reg(?O)
% if the variable is free, bind it to a fresh register
reg(O) --> {nonvar(O)}, !, O.
reg(O) --> {var(O), get_reg_iter(N), string_concat("%", N, O)}, !, O.


% === PRINTING UTILS ===

regs(Os) --> sequence(reg, ", ", Os).

nr_to_res(token(nr(N), _Loc), R) :- number_string(N, R).

indent --> "  ".
newline --> "\n".
