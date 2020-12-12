:- module(parse, [parse/2]).


:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module('tokenize.prolog').
:- use_module('checks.prolog').


parse(File, AST) :-
    tokenize(File, Tokens),
    phrase(program(AST), Tokens), !. % parser is deterministic


% === PROGRAM ===
program(AST) --> sequence(topDef, AST).


topDef(def(RetType, Id, Args, Body)) -->
    current_loc(L),
    complain_on_fail("in top level definition", L),
    get_or_complain(type_(RetType), "expected type"),
    get_or_complain(id(Id), "expected valid id"),
    get_or_complain(s("("), "expected '('"),
    args(Args),
    get_or_complain(s(")"), "expected ')'"),
    blck(Body, [Args]), !.


% === FUNCTION ARGS ===
args(Args) -->
    sequence(arg_, s(","), Args),
    { check_all_args_different(Args) }.

arg_((Id, T)) --> type_(T), id(Id).



% === BLCK ===
blck(Stmts, Cont) -->
    {context_sub(Cont, ContCurr)},
    current_loc(L),
    s("{"),
    complain_on_fail("in block", L),
    blck_stmts(Stmts, ContCurr),
    s("}"), !.

blck_stmts([Stmt|Stmts], Cont) -->
    stmt(Stmt, Cont, ContNext), !,
    blck_stmts(Stmts, ContNext).

blck_stmts([], _) --> [].



% === STMT ===
% stmt(?Stmt, +ContCurr, -ContNext)

stmt(emptStmt, Cont, Cont) --> s(";").                  % Empty
stmt(blckStmt(Stmt), Cont, Cont) --> blck(Stmt, Cont).  % BlockStmt

% IncrStmt
stmt(incrStmt(I), Cont, Cont) -->
    id(I), o("++"), !,
    end_of_stmt,
    {context_get_type(Cont, I, int)}.

% DecrStmt
stmt(decrStmt(I), Cont, Cont) -->
    id(I), o("--"), !,
    end_of_stmt,
    {context_get_type(Cont, I, int)}.

% DeclStmt
stmt(declStmt(Type, Items), Cont, ContNext) -->
    current_loc(L),
    type(Type), !,
    complain_on_fail("in declaration", L),
    sequence(item, s(","), Items),
    end_of_stmt,
    {context_add_items(Items, Type, Cont, ContNext)}.

% AssStmt
stmt(assgStmt(I, E), Cont, Cont) -->
    current_loc(L),
    id(I), o("="), !,
    complain_on_fail("in assignment", L),
    expr(E),
    end_of_stmt,
    { context_type_expr(Cont, T, E),
      context_get_type(Cont, I, T) }.


% RetStmt / VRetStmt
stmt(Ret, Cont, Cont) -->
    current_loc(L),
    return, !,
    complain_on_fail("in return", L),
    (   s(";")
    ->  !, {Ret = rtrnStmt}
    ;   expr(E), s(";"), {Ret = rtrnStmt(E)}
    ).

% if
stmt(Cond, Cont, Cont) -->
    current_loc(L),
    if, !,
    complain_on_fail("in if statement", L),
    s("("), expr(E), s(")"), stmt(S, Cont, _ContIf),
    (   else
    ->  !, stmt(SE, Cont, _ContElse), {Cond = condStmt(E, S, SE)}
    ;   {Cond = condStmt(E, S)}
    ),
    { context_type_expr(Cont, T, E),
      (   T == boolean
      ->  true
      ;   write("'while' condition should evaluate to boolean but is "),
          write(T), nl,
          fail
      )
    }.

% while
stmt(whilStmt(E, S), Cont, Cont) -->
    current_loc(L),
    while, !,
    complain_on_fail("in while", L),
    s("("), expr(E), s(")"), stmt(S, Cont, _ContWhile),
    { context_type_expr(Cont, T, E),
      (   T == boolean
      ->  true
      ;   write("'if' condition should evaluate to boolean but is "),
          write(T), nl,
          fail
      )
    }.

% expr
stmt(exprStmt(E), Cont, Cont) -->
    expr(E),
    end_of_stmt,
    {context_type_expr(Cont, _, E)}.

end_of_stmt --> get_or_complain(s(";"), "expected ';'").


% === ITEM aka L-VALUE
item(Item) -->
    id(I),
    (   (o("="), expr(E))
    ->  !, {Item = ass(I, E)} 
    ;   {Item = lit(I)}
    ).


context_add_items([lit(I)|Items], Type, Cont, ContFinal) :-
    !,
    context_insert(Cont, I, Type, ContNext),
    context_add_items(Items, Type, ContNext, ContFinal).

context_add_items([ass(I, E)|Items], Type, Cont, ContFinal) :-
    !,
    context_insert(Cont, I, Type, ContNext),
    context_type_expr(ContNext, ExprType, E),
    (   Type = ExprType
    ->  !, true
    ;   !,
        write("expression for '"), write(I),
        write("' should be '"), write(Type),
        write("' but is '"), write(ExprType),
        write("'"), nl, fail),
    context_add_items(Items, Type, ContNext, ContFinal).

context_add_items([], _, Cont, Cont).


% === TYPES ===
type_(T) --> type(T).

% TODO(frdrc): complex types

% type_(_) -->
%     id(Id),
%     { term_string(Id, Ids), term_string(Loc, Locs),
%       atomics_to_string(["Unknown type ", Ids, " at location: ", Locs], Msg),
%       throw(Msg) }.



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

not_done, [T] --> [T].

current_loc(Loc), [token(T, Loc)] --> [token(T, Loc)], !.

current_loc_hard(Loc) --> current_loc(Loc), !.
current_loc_hard("end of file") --> [].



% === ERROR MSGS ===

complain_at_loc(S, L) --> { write(S), write(" at "), write(L), nl }.

complain_on_fail(_, _) --> [].
complain_on_fail(S, L) --> complain_at_loc(S, L), {fail}.

get_or_complain(G, _) --> call(G), !.
get_or_complain(_, S) --> current_loc_hard(L), complain_at_loc(S, L), {fail}.


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
