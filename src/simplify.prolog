:- module(simplify, [evaluate_trivial/2]).


evaluate_trivial(expr_bool(true), true).
evaluate_trivial(expr_bool(false), false).
