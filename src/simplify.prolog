:- module(simplify, [evaluate_trivial/2]).


evaluate_trivial(expr_bool(_, true), true).
evaluate_trivial(expr_bool(_, false), false).
