:- use_module(library(apply), [maplist/3, maplist/2]).
:- use_module(library(pure_input), [phrase_from_file/2]).
:- use_module(library(dcg/basics), [string_without/4, integer/3]).
:- use_module(library(lists), [reverse/2, sum_list/2]).
:- use_module(library(clpfd)).

lines([L|Ls]) --> string_without("\n", L), "\n", lines(Ls).
lines([L]) --> string_without("\n", L).

numbers([N|Ns]) --> integer(N), " ", numbers(Ns).
numbers([N]) --> integer(N).

parse_numbers(L, X) :- phrase(numbers(X), L).

delta_(A, B, C) :- C #= B-A.

apply_between(Fn, L0, L) :- apply_between(Fn, L0, [], L).

apply_between(_, [_], L0, L) :- reverse(L0, L).

apply_between(Fn, [R0,R1|Rs], L0, L) :-
	call(Fn, R0, R1, R),
	apply_between(Fn, [R1|Rs], [R|L0], L).

next(L0, Val) :-
	apply_between(delta_, L0, L1),
	reverse(L0, [L|_]),	
	( maplist(#=(0), L1)
	-> Val #= L
	;  next(L1, Val1),
	   Val #= L + Val1
	).

load_data(X) :-
	phrase_from_file(lines(Ls), "/home/max/projects/prolog/aoc/2023/09/input"),
	maplist(parse_numbers, Ls, X).

solve_puzzle(S) :-
	load_data(X0),
	maplist(next, X0, X),
	sum_list(X, S).

solve_puzzleb(S) :-
	load_data(X0),
	maplist(reverse, X0, X1),
	maplist(next, X1, X),
	sum_list(X, S).