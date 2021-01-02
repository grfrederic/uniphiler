:- module(parse, [parse/2]).


:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module('tokenize.prolog').
:- use_module('checks.prolog').
:- use_module('errors.prolog').


parse(File, AST) :-
    tokenize(File, Tokens),
    phrase(program(AST), Tokens), !. % parser is deterministic


parse(_, _) :-
    error_stack_print,
    error_stack_clear,
    fail.


% === PROGRAM ===
program(AST) --> sequence(topDef, AST).


topDef(def(Id, RetType, Args, Body, Loc)) -->
    current_loc(Loc),
    {complain_on_fail("in top level definition", Loc)},
    get_or_complain(type_(RetType), "expected type"),
    get_or_complain(id(Id), "expected valid id"),
    get_or_complain(s("("), "expected '('"),
    get_or_complain(args(Args), "expected args"),
    get_or_complain(s(")"), "expected ')'"),
    get_or_complain(blck(Body), "expected body"), !.


% === FUNCTION ARGS ===
args(Args) -->
    sequence(arg_, s(","), Args).

arg_((Id, T)) --> type_(T), id(Id).



% === BLCK ===
blck(blck(Stmts, Loc)) -->
    current_loc(Loc),
    s("{"),
    {complain_on_fail("in block", Loc)},
    sequence(stmt_cut, Stmts), !,
    s("}"), !.


% === STMT ===
% stmt_cut(?Stmt)
stmt_cut(Stmt) --> stmt(Stmt), !.

% stmt(?Stmt)
stmt(emptStmt) --> s(";").                  % Empty
stmt(blckStmt(Block)) --> blck(Block).      % BlockStmt

% IncrStmt
stmt(incrStmt(I, Loc)) -->
    current_loc(Loc),
    id(I), o("++"), !,
    end_of_stmt.

% DecrStmt
stmt(decrStmt(I, Loc)) -->
    current_loc(Loc),
    id(I), o("--"), !,
    end_of_stmt.

% DeclStmt
stmt(declStmt(Type, Items, Loc)) -->
    current_loc(Loc),
    type(Type), !,
    {complain_on_fail("in declaration", Loc)},
    sequence(item, s(","), Items),
    end_of_stmt.

% AssStmt
stmt(assgStmt(I, E, Loc)) -->
    current_loc(Loc),
    id(I), o("="), !,
    {complain_on_fail("in assignment", Loc)},
    expr(E),
    end_of_stmt.

% RetStmt / VRetStmt
stmt(Ret) -->
    current_loc(Loc),
    return, !,
    {complain_on_fail("in return", Loc)},
    (   s(";")
    ->  !, {Ret = rtrnStmt(Loc)}
    ;   expr(E), s(";"), {Ret = rtrnStmt(E, Loc)}
    ).

% if
stmt(Cond) -->
    current_loc(Loc),
    if, !,
    {complain_on_fail("in if statement", Loc)},
    s("("), expr(E), s(")"), stmt(ST),
    (   else
    ->  !, stmt(SF), {Cond = condStmt(E, ST, SF, Loc)}
    ;   {Cond = condStmt(E, ST, emptStmt, Loc)}
    ).

% while
stmt(whilStmt(E, S, Loc)) -->
    current_loc(Loc),
    while, !,
    {complain_on_fail("in while", Loc)},
    s("("), expr(E), s(")"), stmt(S).

% expr
stmt(exprStmt(E, Loc)) -->
    current_loc(Loc),
    expr(E),
    end_of_stmt.

end_of_stmt --> get_or_complain(s(";"), "expected ';'").


% === ITEM aka L-VALUE
item(Item) -->
    id(I),
    (   (o("="), expr(E))
    ->  !, {Item = ass(I, E)} 
    ;   {Item = lit(I)}
    ).


% === TYPES ===
type_(T) --> type(T).

% TODO(frdrc): complex types



% === EXPR ===
expr(E) --> expr0(E).

expr0(E) --> assocr(expr1, orop, E).
expr1(E) --> assocl(expr2, andop, E).
expr2(E) --> assocl(expr3, relop, E).
expr3(E) --> assocl(expr4, addop, E).
expr4(E) --> assocl(expr5, mulop, E).

expr5(eneg(E)) --> o("-"), !, expr6(E).
expr5(enot(E)) --> o("!"), !, expr6(E).
expr5(E) --> expr6(E).

% just literals and variables
expr6(expr_str(S)) --> [token(lit_str(S), _)], !.
expr6(expr_int(I)) --> [token(lit_int(I), _)], !.
expr6(expr_bool(B)) --> [token(lit_bool(B), _)], !.

expr6(expr_ap(I, Es)) --> id(I), s("("), !, sequence(expr, s(","), Es), s(")").
expr6(expr_id(I)) --> id(I), !.
expr6(expr_in(E)) --> s("("), !, expr(E), s(")").



% === OPS ===
orop(eor) --> o("||").
andop(eand) --> o("&&").

relop(ene) --> o("!=").
relop(eeq) --> o("==").
relop(ele) --> o("<=").
relop(ege) --> o(">=").
relop(elt) --> o("<").
relop(egt) --> o(">").

mulop(emod) --> o("%").
mulop(ediv) --> o("/").
mulop(eprd) --> o("*").

addop(epls) --> o("+").
addop(emin) --> o("-").



% === TOKEN UTILS ===

s(T, L) --> [token(s(T), L)].
s(T) --> s(T, _).

id(I, L) --> [token(id(I), L)].
id(I) --> id(I, _).

type(T, L) --> [token(type(T), L)].
type(T) --> type(T, _).

o(T, L) --> [token(operator(T), L)].
o(T) --> o(T, _).

return(L) --> [token(return, L)].
return --> return(_).

if --> [token(if, _)].
else --> [token(else, _)].
while --> [token(while, _)].

current_loc(Loc), [token(T, Loc)] --> [token(T, Loc)], !.

current_loc_hard(Loc) --> current_loc(Loc), !.
current_loc_hard("end of file") --> [].

get_or_complain(G, _) --> call(G), !.
get_or_complain(_, S) --> current_loc_hard(L), {error(S, L)}.


% === BUILD BINARY OPS ===

assocl(El, Op, Res) --> opsepseq(El, Op, Es, Os), {laf(Os, Es, Res)}.
assocr(El, Op, Res) --> opsepseq(El, Op, Es, Os), {raf(Os, Es, Res)}.


% parse operator separated sequence
opsepseq(El, Op, [E|Es], Os) --> call(El, E), opsepseq_(Es, Os, El, Op).

opsepseq_([E|Es], [O|Os], El, Op) --> call(Op, O), call(El, E), !, opsepseq_(Es, Os, El, Op).
opsepseq_([], [], _, _) --> [].
    

% build expr
laf(Os, [H|T], R) :- laf_(Os, H, T, R).
laf_(_, A, [], A).
laf_([O|Os], A, [H|T], R) :- NA =.. [O, A, H], laf_(Os, NA, T, R).

raf(_, [E], E).
raf([O], [E1, E2], R) :- R =.. [O, E1, E2].
raf([O|Os], [H|T], R) :- raf(Os, T, RT), R =.. [O, H, RT].
