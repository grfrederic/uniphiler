:- module(llvm_print, [llvm_print/3]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).



llvm_print(Llvm) -->
    { init_globals(Llvm) },
    header,
    sequence(topleveldef, newline, Llvm).


header -->
    "declare void @printInt(i32)", newline,
    "declare i32 @readInt()", newline,
    "declare void @printString(i8*)", newline,
    "declare i8* @readString()", newline,
    "declare i8* @concatStrings(i8*, i8*)", newline,
    "declare i1 @compareStrings(i8*, i8*)", newline,
    newline.

topleveldef(constant(Out, Type, Value)) -->
    src(Out), " = constant ", type(Type), sp, value(Value), newline.

topleveldef(func(Type, Id, Regs, Body)) -->
    { init_func(Regs, Body) },
    "define", sp, type(Type), sp, funcName(Id), reg_types(Regs), sp, "{", newline,
    sequence(line_cut, newline, Body), newline,
    (   { Type = void }
    ->  indent, "ret void", newline      % przepraszam
    ;   indent, "unreachable", newline   % naprawdę
    ),
    newline, "}", newline.

reg_types(Regs) --> sequence("(", reg_type, ", ", ")", Regs).
reg_type((Type, _Reg)) --> type(Type).


% LINE

line_cut(Line) --> line(Line), !.

line(call(Type, Id, Args)) --> !,
    indent, "call ", type(Type), " @", Id, sequence("(", argm_typed, ", ", ")", Args).

line(call(Out, Type, Id, Args)) --> !,
    indent, src(Out), " = call ", type(Type), " @", Id, sequence("(", argm_typed, ", ", ")", Args).

line(return(Type, Out)) --> !,
    indent, "ret", sp, type(Type), sp, src(Out).

line(return) --> !,
    indent, "ret void".

line(label(Label)) --> !,
    newline,
    label_set(Label), ":".

line(br(Label)) --> !,
    indent, "br label ", label_ref(Label).

line(br(Cond, LabelTrue, LabelFalse)) --> !,
    indent, "br i1 ", src(Cond), ", label ", label_ref(LabelTrue), ", label ", label_ref(LabelFalse).

line(icmp(Out, LlvmCond, Type, Args)) --> !,
    indent, src(Out), " = icmp ", dumps(LlvmCond), sp, type(Type), sp, sequence(argm, ", ", Args).

line(bitcast(Out, TypeIn, Castee, TypeOut)) --> !,
    indent, src(Out), " = bitcast ", type(TypeIn), sp, src(Castee), " to ", type(TypeOut).

line(phi((Type, Out), SrcLabels)) --> !,
    indent, src(Out), " = phi ", type(Type), sp, phi_args(SrcLabels).

% other builtins
line(X) -->
   { X =.. [Op, Out, Type, Args] }, !,
    indent, src(Out), " = ", dumps(Op), sp, type(Type), sp, sequence(argm, ", ", Args).


line(X) --> dumps(X).  % TODO(frdrc): this should not be necessary


phi_args(SrcLabels) --> sequence(phi_arg, ", ", SrcLabels).
phi_arg(((_Type, Src), Label)) --> "[ ", src(Src), ", ", label_ref(Label), " ]".


label_set(Label) --> Label.
label_ref(Label) --> "%", Label.


funcName(Id) --> "@", Id.

argm(Src) --> src(Src).

argm_typed((Type, Src)) --> type(Type), sp, src(Src).


src(Src) -->
    (   { string(Src) }
    ->  Src
    ;   { term_string(Src, Str) },
        Str
    ).


type(X) --> type_(X), !.  % its simple lookup, dont create backtracking points

type_(int) --> "i32".
type_(boolean) --> "i1".
type_(str) --> "i8*".
type_(char) --> "i8".
type_(void) --> "void".

type_(ptr(T)) --> type_(T), "*".

type_(arr(N, T)) --> "[", number(N), " x ", type(T), "]".


value(X) --> {string(X)}, "c\"", X, "\"".



% === REGISTER GENERATION ===
% We will also need a supply of fresh register names. We handle this
% with a dynamic predicate keeping track of the current variable iteration
% during function generation.
%

init_globals(Llvm) :-
    reset_reg_iter,
    maplist(init_topleveldef, Llvm).


init_topleveldef(constant(Out, _Type, _Value)) :-
    init_glb(Out).

init_topleveldef(func(_Type, _Id, _Regs, _Body)).


init_func(TypeRegs, Body) :-
    reset_reg_iter,
    maplist(init_type_reg, TypeRegs),
    init_body(Body, true).


init_type_reg((_Type, Reg)) :- init_reg(Reg).


% init_body(+Body, +NeedToStartNewBlock)
init_body([label(Label)|Body], true) :- !,
    init_label(Label),
    init_body(Body, false).

init_body([label(Label)|Body], false) :- !,
    init_label(Label),
    init_body(Body, false).

init_body([return|Body], _) :- !,
    init_body(Body, true).

init_body([return(_, _)|Body], _) :- !,
    init_body(Body, true).

init_body(Body, true) :- !,
    init_label(_),  % for javanissen
    init_body(Body, false).

init_body([call(Out, _, _, _)|Body], false) :- !,
    init_reg(Out),
    init_body(Body, false).

init_body([bitcast(Out, _, _, _)|Body], false) :- !,
    init_reg(Out),
    init_body(Body, false).

init_body([icmp(Out, _, _, _)|Body], false) :- !,
    init_reg(Out),
    init_body(Body, false).

init_body([phi((_, Out), _)|Body], false) :- !,
    init_reg(Out),
    init_body(Body, false).

init_body([Line|Body], false) :-
    Line =.. [_Op, Out, _Type, _Args], !,
    init_reg(Out),
    init_body(Body, false).

init_body([_|Body], false) :- !,
    init_body(Body, false).

init_body([], _).


% init_glb(?O)
% if the variable is free, bind it to a fresh number
init_glb(O) :- nonvar(O), !.
init_glb(O) :- var(O), get_reg_iter(N), string_concat("@", N, O).

% init_reg(?O)
% if the variable is free, bind it to a fresh number
init_reg(O) :- nonvar(O), !.
init_reg(O) :- var(O), get_reg_iter(N), string_concat("%", N, O).

% lab(?L)
% if the variable is free, bind it to a fresh number
init_label(L) :- nonvar(L), !.
init_label(L) :- var(L), !, get_reg_iter(N), term_string(N, L).


:- dynamic('reg_iter_'/1).

% reset generation counter to 0
reset_reg_iter :-
    retractall(reg_iter_(_)),
    asserta(reg_iter_(0)).

% get fresh iteration and inc counter
get_reg_iter(N) :-
    reg_iter_(N),
    retractall(reg_iter_(_)),
    N1 is N + 1,
    asserta(reg_iter_(N1)).


% === UTILS ===

dumps(X) --> {term_string(X, S)}, S.

sp --> " ".
indent --> "  ".
newline --> "\n".
