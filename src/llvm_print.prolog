:- module(llvm_print, [llvm_print/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).



llvm_print(Llvm) --> sequence(func, newline, Llvm).


func(func(Type, Id, Args, Body)) -->
    "define", sp, type(Type), sp, funcName(Id), arg_types(Args), sp, "{", newline,
    sequence(line, newline, Body),
    newline, "}", newline.

arg_types(Args) --> sequence("(", arg_type, ", ", ")", Args).
arg_type((_Id, Type)) --> type(Type).


line(call(Out, Id, Args)) --> !,
    indent, dumps(Out), " = call @", Id, sequence("(", argm, ", ", ")", Args).

line(return(Out)) --> !,
    indent, "return", sp, dumps(Out).

line(return) --> !,
    indent, "return".

line(X) --> indent,  dumps(X).


funcName(Id) --> "@", Id.
argm(X) --> dumps(X).


type(int) --> "i32".
type(bool) --> "i1".
type(str) --> "i8*".
type(void) --> "void".


dumps(X) --> {term_string(X, S)}, S.


sp --> " ".
indent --> "  ".
newline --> "\n".
