:- use_module(library(pure_input), [phrase_from_file/2]).
:- use_module(library(dcg/basics), [string_without/4, integer/3, 
									remainder/3]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [sum_list/2]).
:- use_module(library(apply), [maplist/3]).

lines([L|Ls]) --> string_without("\n", L), "\n", lines(Ls).
lines([L]) --> string_without("\n", L).
lines([]) --> [].

numbers([N|Ns]) --> integer(N), ",", numbers(Ns).
numbers([N]) --> integer(N).

operational --> ".".
operational --> "?".
damaged --> "#".
damaged --> "?".

n_operational(N) --> operational, n_operational(N0), {N #= N0+1}.
n_operational(1) --> operational.

n_damaged(N) --> damaged, n_damaged(N0), {N #= N0+1}.
n_damaged(1) --> damaged.

n_operational_damaged([D0|D], [O0|O]) --> n_operational(O0), n_damaged(D0), n_operational_damaged(D, O).
n_operational_damaged([], [O]) --> n_operational(O).
n_operational_damaged([], []) --> [].

n_damaged_operational([D0|D], [O0|O]) --> n_damaged(D0), n_operational(O0), n_damaged_operational(D, O).
n_damaged_operational([D], []) --> n_damaged(D).
n_damaged_operational([], []) --> [].

springs(S, O) --> n_operational_damaged(S, O).
springs(S, O) --> n_damaged_operational(S, O).

springs_numbers(N) --> string_without(" ", S0), " ", numbers(N), { phrase(springs(N, _), S0) }.

apply_springs_numbers(L, N) :-
	phrase(springs_numbers(N), L).

count_mappings(L, N) :-
	findall(_, apply_springs_numbers(L, _), Xs),
	length(Xs, N).

folded_string(_, _, 0) --> [].
folded_string(X, _, 1) --> X.
folded_string(X, S, N) --> {N #> 1, N0 #= N-1}, X, S, folded_string(X, S, N0).

unfold_springs_numbers(X) -->
	string_without(" ", S0),
	" ",
	remainder(N0),
	{ phrase(folded_string(S0, "?", 5), S1),
	  phrase(folded_string(N0, ",", 5), N1),
	  phrase((S1, " ", N1), X)}.

apply_unfold_springs_numbers(S0, S1) :-
	phrase(unfold_springs_numbers(S1), S0).

load_data(L, N) :-
	phrase_from_file(lines(L), "/home/max/projects/prolog/aoc/2023/12/input"),
	maplist(count_mappings, L, N0),
	sum_list(N0, N).

apply_unfold_count(L0, C) :-
	apply_unfold_springs_numbers(L0, L),
	count_mappings(L, C).

load_data2(L0, N) :-
	phrase_from_file(lines(L0), "/home/max/projects/prolog/aoc/2023/12/input"),
	maplist(apply_unfold_count, L0, N0),
	sum_list(N0, N).