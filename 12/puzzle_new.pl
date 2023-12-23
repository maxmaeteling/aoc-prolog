:- use_module(library(apply), [maplist/4, maplist/2, maplist/3]).
:- use_module(library(pure_input), [phrase_from_file/2]).
:- use_module(library(dcg/basics), [string_without/4, integer/3, 
								  remainder/3]).
:- use_module(library(lists), [append/3, sum_list/2]).
:- use_module(library(clpfd)).

:- table spring_version/3.

lines([L|Ls]) --> string_without("\n", L), "\n", lines(Ls).
lines([L]) --> string_without("\n", L).
lines([]) --> [].

numbers([N|Ns]) --> integer(N), ",", numbers(Ns).
numbers([N]) --> integer(N).

operational(o) --> ".".
damaged(d) --> "#".
unknown(u) --> "?".

spring(X) --> operational(X) | damaged(X) | unknown(X).

springs([X|Xs]) --> spring(X), springs(Xs).
springs([X]) --> spring(X).

springs_numbers(S, N) -->
	string_without(" ", S0),
	" ",
	remainder(N0),
	{   phrase(springs(S), S0),
		phrase(numbers(N), N0)
	}.

parse_springs_numbers(L, [o|S], N) :-
	phrase(springs_numbers(S, N), L).

mb_operational(o).
mb_operational(u).
mb_damaged(d).
mb_damaged(u).

spring_version([u|Ss0], [N|Ns], Cnt) :-
	spring_version([o|Ss0], [N|Ns], Cnt).

spring_version([o|Ss0], [N|Ns], Cnt) :-
	length(S0, N),
	append(S0, Ss, Ss0),
	maplist(mb_damaged, S0),
	spring_version(Ss, Ns, Cnt0),
	spring_version(Ss0, [N|Ns], Cnt1),
	Cnt is Cnt0+Cnt1.

spring_version([o|Ss0], [N0|Ns], Cnt) :-
	length(Ss0, N),
	N < N0,
	spring_version(Ss0, [N0|Ns], Cnt).

spring_version([o|Ss0], [N|Ns], Cnt) :-
	length(S0, N),
	append(S0, _, Ss0),
	memberchk(o, S0),
	spring_version(Ss0, [N|Ns], Cnt).

spring_version(Ss, [], 1) :-
	maplist(mb_operational, Ss).

spring_version(Ss, [], 0) :-
	memberchk(d, Ss).

spring_version([d|_], _, 0).
spring_version([], [_|_], 0).

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

solve_puzzle(Cs) :-
	abolish_all_tables,
	phrase_from_file(lines(L0), "/home/max/projects/prolog/aoc/2023/12/input"),
	maplist(parse_springs_numbers, L0, S, N),
	spring_version_loop(S, N, 0, Cs).

solve_puzzleb(Cs) :-
	abolish_all_tables,
	phrase_from_file(lines(L0), "/home/max/projects/prolog/aoc/2023/12/input"),
	maplist(apply_unfold_springs_numbers, L0, L),
	maplist(parse_springs_numbers, L, S, N),
	spring_version_loop(S, N, 0, Cs).

spring_version_loop([], _, C, C).
spring_version_loop([S|Ss], [N|Ns], C0, C) :-
	spring_version(S, N, C1),
	C2 #= C0+C1,
	spring_version_loop(Ss, Ns, C2, C).