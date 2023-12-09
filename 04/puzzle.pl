:- autoload(library(dcg/basics), [string_without/4, integer/3]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(ordsets), [ord_intersect/3]).
:- autoload(library(pure_input), [phrase_from_file/2]).
:- autoload(library(lists), [sum_list/2, min_list/2, append/3, 
							 reverse/2]).

lines([L|Ls]) --> string_without("\n", L), "\n", lines(Ls).
lines([L]) --> string_without("\n", L).
lines([]) --> [].

space --> " ".

spaces --> space, spaces.
spaces --> space.

card(c(I, N0, N1)) --> "Card", spaces, integer(I), ":", spaces, numbers(N0), spaces, "|", spaces, numbers(N1).

numbers([N]) --> integer(N).
numbers([N|Ns]) --> integer(N), spaces, numbers(Ns).

parse_card(L, C) :- phrase(card(C), L).

calc_score(c(_, N0, N1), Len) :-
	sort(N0, N00),
	sort(N1, N10),
	ord_intersect(N00, N10, N2),
	length(N2, Len).

calc_score_exp(C, S) :-
	calc_score(C, Len),
	(   Len = 0 ->
		S is 0
	;	S is 2**(Len-1)
	).

load_data(D) :-
	phrase_from_file(lines(L), "/home/max/projects/prolog/aoc/2023/04/input"),
	maplist(parse_card, L, D).

solve_puzzle(S) :-	
	load_data(D),
	maplist(calc_score_exp, D, S0),
	sum_list(S0, S).

card_n_score(C, [1, S]) :-
	calc_score(C, S).

first(L, E) :- nth0(0, L, E).

solve_puzzle_b(C) :-
	load_data(C0),
	maplist(card_n_score, C0, C1),
	process_numbers(C1, [], C2),
	maplist(first, C2, C3),
	sum_list(C3, C).

update_counts(A, [N0, Sc], [N, Sc]) :- N is N0 + A.

process_numbers([], S0, S) :- reverse(S0, S).
process_numbers([[N, Sc]|Cs0], S0, S) :-
	length(Cs0, Len0),
	min_list([Len0, Sc], LenUpd),
	length(Upd0, LenUpd),
	append(Upd0, Cs1, Cs0),
	maplist(update_counts(N), Upd0, Upd),
	append(Upd, Cs1, Cs),
	process_numbers(Cs, [[N, Sc]|S0], S).