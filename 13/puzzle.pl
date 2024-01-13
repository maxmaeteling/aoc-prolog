:- use_module(library(lists), [append/3, reverse/2, 
							   sum_list/2]).
:- use_module(library(pure_input), [phrase_from_file/2]).
:- use_module(library(apply), [maplist/2, maplist/3]).
:- use_module(library(clpfd)).

field(0) --> ".".
field(1) --> "#".

fields([F|Fs]) --> field(F), fields(Fs).
fields([F]) --> field(F).

field_lines([L|Ls]) --> fields(L), "\n", field_lines(Ls).
field_lines([L]) --> fields(L).

patterns([P|Ps]) --> field_lines(P), "\n\n", patterns(Ps).
patterns([P]) --> field_lines(P).

list_reflect_l(L, N) :-
	append(LL, LR, L),
	reverse(LLR, LR),
	LLR = [_|_],
	append(_, LLR, LL),
	length(LL, N).

list_reflect_r(L0, N) :-
	length(L0, N0),
	reverse(L0, L),
	list_reflect_l(L, N1),
	N #= N0-N1.

list_reflect_l_(L, N) :- list_reflect_l(N, L).
list_reflect_r_(L, N) :- list_reflect_r(N, L).

horizontal_reflection(B, N) :-
	maplist(list_reflect_l_(N), B) ; maplist(list_reflect_r_(N), B).

vertical_reflection(B0, N) :-
	transpose(B0, B),
	horizontal_reflection(B, N0),
	N #= N0*100.

reflection(D, N) :-
	horizontal_reflection(D, N)	; vertical_reflection(D, N).

invert(0, 1).
invert(1, 0).

invert_smudge(P0, P) :-
	append(L, [X0|R], P0),
	append(XL, [XX0|XR], X0),
	invert(XX0, XX),
	append(XL, [XX|XR], X),
	append(L, [X|R], P).

second_reflection(P0, N) :-
	reflection(P0, N0),
	invert_smudge(P0, P),
	reflection(P, N),
	N #\= N0.

load_data(Ds) :-
	phrase_from_file(patterns(Ds), "/home/max/projects/prolog/aoc/2023/13/input").

solve_puzzle(R) :-
	load_data(Ds),
	maplist(reflection, Ds, Ns0),
	sum_list(Ns0, R).

solve_puzzleb(R) :-
	load_data(Ds),
	maplist(second_reflection, Ds, Ns0),
	sum_list(Ns0, R).