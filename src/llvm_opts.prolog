:- module(llvm_opts, [llvm_opts/2]).

:- use_module('daddy.prolog').


llvm_opts(Llvm, LlvmOptd) :-
    include(const, Llvm, ConstantDecls),
    exclude(const, Llvm, FunctionDecls),
    remove_duplicate_constant_decls(ConstantDecls, ConstantDeclsOptd),
    maplist(function_opts, FunctionDecls, FunctionDeclsOptd),
    append(ConstantDeclsOptd, FunctionDeclsOptd, LlvmOptd).


const(constant(_, _, _)).


remove_duplicate_constant_decls(ConstantDecls, ConstantDeclsOptd) :-
    maplist(putl(ConstantDeclsOptd), ConstantDecls),
    clsl(ConstantDeclsOptd).

putl(L, X) :- member(X, L), !.
clsl(L) :- length(L, _), !.



function_opts(
  func(RetType, Id, Regs, LlvmBlocks),
  func(RetType, Id, Regs, LlvmBlocksOptd2)
) :-
    exclude(trivialBlock, LlvmBlocks, LlvmBlocksOptd1),
    gcse(LlvmBlocksOptd1, LlvmBlocksOptd2).

trivialBlock(llvmBlock(_Label, [unreachable])).


% === GCSE ===
% we use dynamic programming to avoid an explicit topological sort

gcse(LlvmBlocks, LlvmBlocksOptd) :-
    init_gcse_tab(LlvmBlocks, LlvmBlocksOptd, GcseTab),
    run_gcse_tab(GcseTab).


init_gcse_tab(LlvmBlocks, LlvmBlocksOptd, GcseTab) :-
    maplist(init_gcse_row, LlvmBlocks, LlvmBlocksOptd, GcseTab).

init_gcse_row(Block, BlockOptd, (Block, (_Is, _Os, _Ds), BlockOptd, _Mem)).


run_gcse_tab(GcseTab) :-
    maplist(get_label, GcseTab, Labels),
    compute_connections(GcseTab),
    maplist(compute_opts(GcseTab), Labels).


% compute daddy dominators

compute_connections(GcseTab) :-
    maplist(compute_outs, GcseTab),
    maplist(compute_ins(GcseTab), GcseTab),
    compute_daddies(GcseTab).

compute_outs((llvmBlock(_Label, Lines), (_, Os, _), _, _)) :-
    append(_, [br(Out)], Lines), !,
    Os = [Out].

compute_outs((llvmBlock(_Label, Lines), (_, Os, _), _, _)) :-
    append(_, [br(_, Out1, Out2)], Lines), !,
    Os = [Out1, Out2].

compute_outs((_, (_, [], _), _, _)) :- !.


compute_ins(GcseTab, (llvmBlock(Label, _Lines), (Is, _, _), _, _)) :-
    include(in_outs(Label), GcseTab, InRows),
    maplist(get_label, InRows, Is).


compute_daddies(GcseTab) :-
    prep(GcseTab, Graph, AllDaddies),
    compute_daddies(Graph, AllDaddies).


prep(GcseTab, Graph, AllDaddies) :-
    maplist(prep_, GcseTab, Graph, AllDaddies).

prep_((llvmBlock(Label, _), (Is, _, Ds), _, _), (Label, Is), (Label, Ds)).



% compute opts

compute_opts(GcseTab, Label) :-
    get_row(Label, GcseTab, (_, _, BlckOpt, _)),
    nonvar(BlckOpt),
    !.

compute_opts(GcseTab, Label) :-
    get_row(Label, GcseTab, (Blck, (_, _, Ds), BlckOpt, MemNew)),
    hard_drop(Label, Ds, DsNoSelf),
    maplist(compute_opts(GcseTab), DsNoSelf),
    maplist(get_mem(GcseTab), DsNoSelf, Mems),
    union_mems(Mems, Mem),
    run_opt(Blck, BlckOpt, Mem, MemNew).

% run opt for block
run_opt(llvmBlock(Label, Lines), llvmBlock(Label, LinesOptd), Mem, MemNew) :-
    ro_(Lines, LinesOptd, Mem, MemNew).

% we can ignore phis that merge the same register
ro_([phi((_, V1), [((_, V1), _), ((_, V2), _)])|Ls], Los, Mem, MemNew) :-
    V1 == V2, !,
    ro_(Ls, Los, Mem, MemNew).

% ... or phis that merge a register on itself
ro_([phi((_, V0), [((_, V1), _), ((_, V2), _)])|Ls], Los, Mem, MemNew) :-
    (V0 == V1; V0 == V2), !,
    ro_(Ls, Los, Mem, MemNew).


% reuse already computed stuff
ro_([L|Ls], Los, Mem, MemNew) :-
    (   not_memorable(L), !
    ->  Los = [L|LosRest], ro_(Ls, LosRest, Mem, MemNew)
    ;   (   in_memory(L, Mem)
        ->  ro_(Ls, Los, Mem, MemNew)
        ;   Los = [L|LosRest],
            MemNew = [L|MemNewRest],
            ro_(Ls, LosRest, [L|Mem], MemNewRest)
        )
    ).

ro_([], [], _Mem, []) :- !.


% memory utils

% dont use for gcse
not_memorable(unreachable).
not_memorable(return).
not_memorable(return(_, _)).
not_memorable(br(_)).
not_memorable(br(_, _, _)).
not_memorable(call(_, _, _)).
not_memorable(call(_, _, _, _)).
not_memorable(call(_, _, _, _)).


in_memory(L, [ML|_Mem]) :- match_memory(L, ML), !.
in_memory(L, [_|Mem]) :- in_memory(L, Mem).


match_memory(
    phi((Type, Out1), SrcLabels1),
    phi((Type, Out2), SrcLabels2)
) :-
    SrcLabels1 == SrcLabels2,
    Out1 = Out2.

match_memory(L1, L2) :-
    L1 =.. [Op, Out1|Rest1],
    L2 =.. [Op, Out2|Rest2],
    Rest1 == Rest2,
    Out1 = Out2.


union_mems(Ms, U) :- foldl(um2_, Ms, [], U).

um2_([L|M1], M2, U) :-
    hard_member(L, M2), !,
    um2_(M1, M2, U).

um2_([L|M1], M2, [L|U]) :-
    um2_(M1, M2, U).

um2_([], M2, M2) :- !.



% gcse tab utils
in_outs(Label, (_, (_, Os, _), _, _)) :- hard_member(Label, Os).

has_label_hard(Label, (llvmBlock(LabelBlock, _Lines), _, _, _)) :-
    Label == LabelBlock.

get_label((llvmBlock(Label, _Lines), _, _, _), Label).

get_row(Label, GcseTab, Row) :-
    include(has_label_hard(Label), GcseTab, [Row]).

get_mem(GcseTab, Label, Mem) :-
    include(has_label_hard(Label), GcseTab, [(_, _, _, Mem)]).


% === UTILS ===
% like member, but only for check and uses == not =

% hard_member(+X, +L).
hard_member(X, [Y|_]) :- X == Y.
hard_member(X, [_|L]) :- hard_member(X, L).


% hard_drop(+X, +L1, +L2).
hard_drop(X, [Y|L1], L2) :- X == Y, !, hard_drop(X, L1, L2).
hard_drop(X, [Y|L1], [Y|L2]) :- hard_drop(X, L1, L2).
hard_drop(_, [], []).
