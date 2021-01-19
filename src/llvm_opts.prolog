:- module(llvm_opts, [llvm_opts/2]).


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

init_gcse_row(Block, BlockOptd, (Block, _Ins, _Outs, BlockOptd, _Mem)).


run_gcse_tab(GcseTab) :-
    maplist(get_label, GcseTab, Labels),
    maplist(compute_outs, GcseTab),
    maplist(compute_ins(GcseTab), GcseTab),
    maplist(compute_opts(GcseTab), Labels).


% compute outs

compute_outs((llvmBlock(_Label, Lines), _, Outs, _, _)) :-
    append(_, [br(Out)], Lines), !,
    Outs = [Out].

compute_outs((llvmBlock(_Label, Lines), _, Outs, _, _)) :-
    append(_, [br(_, Out1, Out2)], Lines), !,
    Outs = [Out1, Out2].

compute_outs((_, _, [], _, _)) :- !.


% compute ins
compute_ins(GcseTab, (llvmBlock(Label, _Lines), Ins, _, _, _)) :-
    include(in_outs(Label), GcseTab, InRows_),
    exclude(has_label_hard(Label), InRows_, InRows),  % exclude self
    maplist(get_label, InRows, Ins).


% compute opts
compute_opts(GcseTab, Label) :-
    get_row(Label, GcseTab, (_, _, _, BlckOpt, _)),
    nonvar(BlckOpt),
    !.

compute_opts(GcseTab, Label) :-
    get_row(Label, GcseTab, (Blck, Ins, _, BlckOpt, MemOut)),
    BlckOpt = llvmBlock(_, _),  % lock to avoid loops
    maplist(compute_opts(GcseTab), Ins),
    maplist(get_mem(GcseTab), Ins, MemsOrLoops),
    exclude(var, MemsOrLoops, Mems),
    merge_mems(Mems, MemIn),
    run_opt(Blck, BlckOpt, MemIn, MemOut).


% run opt for block
run_opt(llvmBlock(Label, Lines), llvmBlock(Label, LinesOptd), Mem, MemOut) :-
    ro_(Lines, LinesOptd, Mem, MemOut).


% we can ignore phis that merge the same register
ro_([phi((_, V1), [((_, V1), _), ((_, V2), _)])|Ls], Los, Mem, MemOut) :-
    V1 == V2,
    ro_(Ls, Los, Mem, MemOut).


% reuse already computed stuff
ro_([L|Ls], Los, Mem, MemOut) :-
    (   not_memorable(L), !
    ->  Los = [L|LosRest], ro_(Ls, LosRest, Mem, MemOut)
    ;   (   in_memory(L, Mem)
        ->  ro_(Ls, Los, Mem, MemOut)
        ;   Los = [L|LosRest], ro_(Ls, LosRest, [L|Mem], MemOut)
        )
    ).

ro_([], [], Mem, Mem) :- !.


% memory utils

% can be
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


merge_mems([], []) :- !.
merge_mems([M|Ms], Mrg) :- !,
    mm_(M, Ms, Mrg).

mm_([L|M], Ms, [L|Mrg]) :-
    maplist(hard_member(L), Ms), !,
    mm_(M, Ms, Mrg).

mm_([_|M], Ms, Mrg) :- !,
    mm_(M, Ms, Mrg).

mm_([], _, []) :- !.


% gcse tab utils
in_outs(Label, (_, _, Outs, _, _)) :- hard_member(Label, Outs).

has_label_hard(Label, (llvmBlock(LabelBlock, _Lines), _, _, _, _)) :-
    Label == LabelBlock.

get_label((llvmBlock(Label, _Lines), _, _, _, _), Label).

get_row(Label, GcseTab, Row) :-
    include(has_label_hard(Label), GcseTab, [Row]).

get_mem(GcseTab, Label, Mem) :-
    include(has_label_hard(Label), GcseTab, [(_, _, _, _, Mem)]).


% === UTILS ===
% like member, but only for check and uses == not =

% hard_member(+X, +L).
hard_member(X, [Y|_]) :- X == Y.
hard_member(X, [_|L]) :- hard_member(X, L).
