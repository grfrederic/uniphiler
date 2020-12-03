:- module(parse, [parse/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- use_module('tokenize.prolog').


parse(File, AST) :-
    tokenize(File, Tokens),
    write(Tokens),  % TODO(frdrc): just debug
    phrase(program(AST), Tokens), !. % parser is deterministic

parse(_, _) :-
    throw(parser_failed).


% === PROGRAM ===
program(AST) --> sequence(topDef, AST).


topDef(def(RetType, Id, Args, Body)) -->
    type_(RetType), id(Id), sp("("), args(Args), sp(")"), blck(Body).


% === FUNCTION ARGS ===
args(Args) --> sequence(arg_, sp(","), Args).
arg_(ar(T, Id)) --> type_(T), id(Id).


% === BLCK ===
blck(Stmts) --> sp("{"), sequence(stmt, Stmts), sp("}").


% === STMT ===
stmt(emptStmt) --> sp(";").                                             % Empty
stmt(blckStmt(Stmt)) --> blck(Stmt).                                    % BlockStmt
stmt(declStmt(Items)) --> sequence(item, sp(","), Items), sp(";").      % DeclStmt
stmt(assgStmt(I, E)) --> id(I), sp("="), expr(E), sp(";").              % AssStmt
stmt(incrStmt(I)) --> id(I), sp("+"), sp("+"), sp(";").                 % IncrStmt
stmt(decrStmt(I)) --> id(I), sp("-"), sp("-"), sp(";").                 % DecrStmt
stmt(rtrnStmt(E)) --> id("return"), expr(E), sp(";").                   % RetStmt
stmt(rtrnStmt) --> id("return"), sp(";").                               % VRetStmt

% if
stmt(condStmt(E, S)) -->
    id("if"), sp("("), expr(E), sp(")"), stmt(S).

% if-else
stmt(condStmt(E, S1, S2)) -->
    id("if"), sp("("), expr(E), sp(")"), stmt(S1), id("else"), stmt(S2).

% while
stmt(whilStmt(E, S)) -->
    id("while"), sp("("), expr(E), sp(")"), stmt(S).

% expr
stmt(exprStmt(E)) -->
    expr(E), sp(";").


% === ITEM aka L-VALUE
item(lit(I)) --> id(I).
item(ass(I, E)) --> id(I), sp("="), expr(E).


% === TYPES ===
type_(type("int")) --> id("int").
type_(type("string")) --> id("string").
type_(type("boolean")) --> id("boolean").
type_(type("void")) --> id("void").

type_(_) -->
    id(Id),
    { term_string(Id, Ids), term_string(Loc, Locs),
      atomics_to_string(["Unknown type ", Ids, " at location: ", Locs], Msg),
      throw(Msg) }.


% === EXPR ===
expr(int(N)) --> nr(N).


%-- Expressions ---------------------------------------------
%Neg.       Expr5 ::= "-" Expr6 ;
%Not.       Expr5 ::= "!" Expr6 ;
%EMul.      Expr4 ::= Expr4 MulOp Expr5 ;
%EAdd.      Expr3 ::= Expr3 AddOp Expr4 ;
%ERel.      Expr2 ::= Expr2 RelOp Expr3 ;
%EAnd.      Expr1 ::= Expr2 "&&" Expr1 ;
%EOr.       Expr ::= Expr1 "||" Expr ;

% % === EXPRESSIONS ===
% 
% exp(E) --> exp1(E).
% 
% % addition is right assoc
% exp1(exp_add(E1, E2)) --> exp2(E1), t(plus_sign), !, exp1(E2).
% exp1(T) --> exp2(T).
% 
% % mul/div also factor out tail for left assoc
% exp3(E) --> exp4(H), exp3tail(H, E).
% 
% exp3tail(A, E) --> t(asterisk), !, exp4(S), exp3tail(exp_mul(A, S), E).
% exp3tail(A, E) --> t(slash), !, exp4(S), exp3tail(exp_div(A, S), E).
% exp3tail(A, A) --> [].
% 

% just literals and variables
%EVar.      Expr6 ::= Ident ;
%ELitInt.   Expr6 ::= Integer ;
%ELitTrue.  Expr6 ::= "true" ;
%ELitFalse. Expr6 ::= "false" ;
%EApp.      Expr6 ::= Ident "(" [Expr] ")" ;
%EString.   Expr6 ::= String ;
expr4(expr_id(I)) --> id(I), !.
expr6(expr_nr(I)) --> nr(I), !.
expr6(true) --> id("true"), !.
expr6(false) --> id("false"), !.
expr6(expr_ap(I, Es)) --> id(I), sp("("), sequence(expr, sp(","), Es), sp(")").
expr6(expr_in(E)) --> sp("("), expr(E), sp(")").


% === TOKEN UTILS ===

sp(Special) --> [token(sp(Special), _)].
nr(token(nr(I), Loc)) --> [token(nr(I), Loc)].

id(I) --> id(I, _).
id(I, Loc) --> [token(id(I), Loc)].


% === DEBUG ===
:- trace(topDef).
:- trace(blck).
:- trace(stmt).
%:- trace(type_).
%:- trace(sp).
