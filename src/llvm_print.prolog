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


% LINE

line(call(Out, Type, Id, Args)) --> !,
    indent, dumps(Out), " = call ", type(Type), " @", Id, sequence("(", argm_typed, ", ", ")", Args).

line(return(Type, Out)) --> !,
    indent, "return", sp, type(Type), sp, dumps(Out).

line(return) --> !,
    indent, "return".

line(label(Label)) --> !,
    newline,
    dumps(Label), ":".

line(br(Label)) --> !,
    indent, "br label ", label(Label).

line(br(Cond, LabelTrue, LabelFalse)) --> !,
    indent, "br i1 ", dumps(Cond), ", label ", label(LabelTrue), ", label ", label(LabelFalse).

line(icmp(Out, LlvmCond, Type, Args)) --> !,
    indent, dumps(Out), " = icmp ", dumps(LlvmCond), sp, type(Type), sp, sequence(argm, ", ", Args).

line(phi((Type, Out), SrcLabels)) --> !,
    indent, dumps(Out), " = phi ", type(Type), sp, phi_args(SrcLabels).

% other builtins
line(X) -->
    { X =.. [Op, Out, Type, Args] }, !,
    indent, dumps(Out), " = ", dumps(Op), sp, type(Type), sp, sequence(argm, ", ", Args).


line(X) --> dumps(X).  % TODO(frdrc): this should not be necessary



phi_args(SrcLabels) --> sequence(phi_arg, ", ", SrcLabels).
phi_arg(((_Type, Src), Label)) --> "[ ", dumps(Src), ", ", label(Label), " ]".


label(Name) --> "%", dumps(Name).
funcName(Id) --> "@", Id.

argm(Src) --> dumps(Src).
argm_typed((Type, Src)) --> type(Type), sp, dumps(Src).


type(int) --> "i32".
type(boolean) --> "i1".
type(str) --> "i8*".
type(void) --> "void".


dumps(X) --> {term_string(X, S)}, S.


sp --> " ".
indent --> "  ".
newline --> "\n".
