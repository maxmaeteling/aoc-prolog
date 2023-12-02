:- use_module(library(dcg/basics)).

lines([L|Ls]) --> string_without("\n", L), "\n", lines(Ls).
lines([L]) --> string_without("\n", L).
lines([]) --> [].

seq([]) --> [].
seq([C|Cs]) --> [C], seq(Cs).

number_chars(0) --> "zero".
number_chars(1) --> "one".
number_chars(2) --> "two".
number_chars(3) --> "three".
number_chars(4) --> "four".
number_chars(5) --> "five".
number_chars(6) --> "six".
number_chars(7) --> "seven".
number_chars(8) --> "eight".
number_chars(9) --> "nine".

number(C) --> [C0], {C is C0 - 0'0, C >= 0 , C =< 9}.
number_num_chars(C) --> number_chars(C) | number(C).

first_number(Fn, C) --> call(Fn, C), seq(_).
first_number(Fn, C) --> [_], first_number(Fn, C).

last_number(Fn, C) --> seq(_), call(Fn, C).
last_number(Fn, C) --> last_number(Fn, C), [_].

first_last_number(Fn, Cs, F, L) :-
	phrase(first_number(Fn, F), Cs),
	phrase(last_number(Fn, L), Cs).

checksum(F, L, X) :-
	X is 10 * F + L.

solve_puzzle_(Fn, S) :-
	phrase_from_file(lines(D0), "/home/max/projects/prolog/aoc/2023/01/input"),
	maplist(first_last_number(Fn), D0, F, L),
	maplist(checksum, F, L, D1),
	sum_list(D1, S).

solve_puzzle(D) :-
	solve_puzzle_(number, D).

solve_puzzle_b(D) :-
	solve_puzzle_(number_num_chars, D).