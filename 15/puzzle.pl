:- use_module(library(pure_input), [phrase_from_file/2]).
:- use_module(library(dcg/basics), [string_without/4, integer/3]).
:- use_module(library(clpfd)).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [sum_list/2, nth0/3, nth1/3, append/3, 
							   reverse/2]).
:- use_module(library(assoc), [empty_assoc/1, get_assoc/3, put_assoc/4, 
							   assoc_to_list/2]).

hash([], R, R).
hash([N|Ns], R0, R) :-
	R1 #= R0+N,
	R2 #= R1*17,
	R3 #= R2 rem 256,
	hash(Ns, R3, R).

hash(Ns, R) :- hash(Ns, 0, R).

steps([S|Ss]) --> string_without(",", S), ",", steps(Ss).
steps([S]) --> string_without(",", S).

load_data(X) :-
	phrase_from_file(steps(X), "/home/max/projects/prolog/aoc/2023/15/input").

solve_puzzle(X) :-
	load_data(X0),
	maplist(hash, X0, R),
	sum_list(R, X).

seq(s(rem, L)) --> string_without("-", L), "-".
seq(s(upd, L, F)) --> string_without("=", L), "=", integer(F).

parse_seq(L, X) :- phrase(seq(X), L).

box_lense(B0, H, s(rem, L), B) :-
	get_assoc(H, B0, V0),
	findall(X, (nth0(_, V0, X), X = l(L0, _), L0 \= L), V1),
	put_assoc(H, B0, V1, B).

box_lense(B0, H, s(rem, _), B0) :-
	\+ get_assoc(H, B0, _).

box_lense(B0, H, s(upd, L, F), B) :-
	\+ get_assoc(H, B0, _),
	put_assoc(H, B0, [l(L, F)], B).

box_lense(B0, H, s(upd, L, F), B) :-
	get_assoc(H, B0, V1),
	insert_lenses(V1, l(L, F), V),
	put_assoc(H, B0, V, B).

insert_lenses(V0, l(L, F), V1) :-
	nth0(_, V0, l(L, _)),
	append(H, [l(L, _)|T], V0),
	append(H, [l(L, F)|T], V1).

insert_lenses(V0, l(L, F), [l(L, F)|V0]) :-
	\+ nth0(_, V0, l(L, _)).

box_lenses([], _, B, B).
box_lenses([H|Hs], [L|Ls], B0, B) :-
	box_lense(B0, H, L, B1),
	box_lenses(Hs, Ls, B1, B).

score(K-V0, S) :-
	reverse(V0, V),
	findall(X, (nth1(N, V, l(_, F)), X #= N*F*(1+K)), Xs),
	sum_list(Xs, S).

hash_label(s(rem, L0), L) :- hash(L0, L).
hash_label(s(upd, L0, _), L) :- hash(L0, L). 

solve_puzzleb(S) :-
	load_data(X0),
	maplist(parse_seq, X0, X),
	maplist(hash_label, X, H),
	empty_assoc(B0),
	box_lenses(H, X, B0, B),
	assoc_to_list(B, L),
	maplist(score, L, S0),
	sum_list(S0, S).