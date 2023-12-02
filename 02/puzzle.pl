:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

seq([]) --> [].
seq([C|Cs]) --> [C], seq(Cs).

lines([L|Ls]) --> string_without("\n", L), "\n", lines(Ls).
lines([L]) --> string_without("\n", L).
lines([]) --> [].

cube_color(r) --> "red".
cube_color(b) --> "blue".
cube_color(g) --> "green".

game_id(I) --> "Game ", integer(I), ": ".

cube(N, C) --> integer(N), " ", cube_color(C).

cube_set([c(N, C)|Cs]) --> cube(N, C), ", ", cube_set(Cs).
cube_set([c(N, C)]) --> cube(N, C).

cube_sets([S|Ss]) --> cube_set(S), "; ", cube_sets(Ss).
cube_sets([S]) --> cube_set(S).

game(set(I, S)) --> game_id(I), cube_sets(S).

parse_game(L, S) :-
	phrase(game(S), L).

max_cubes_(C, Col, N) :-
	findall(X, member(c(X, Col), C), Xs),
	max_list([0|Xs], N).

max_cubes(C, R, G, B) :-
	maplist(max_cubes_(C), [r, g, b], [R, G, B]).

game_valid(set(_, G)) :-
	maplist(max_cubes, G, Rs, Gs, Bs),
	maplist(max_list, [Rs, Gs, Bs], [Rm, Gm, Bm]),
	12 >= Rm,
	13 >= Gm,
	14 >= Bm.

game_min_cubes(set(_, S), Sc) :-
	maplist(max_cubes, S, Rs, Gs, Bs),
	maplist(max_list, [Rs, Gs, Bs], [R, G, B]),
	Sc is R * G * B.

game_id(set(I, _), I).

load_data(S) :-
	phrase_from_file(lines(X0), "/home/max/projects/prolog/aoc/2023/02/input"),
	maplist(parse_game, X0, S).

solve_puzzle(Score) :-
	load_data(S),
	setof(X, (member(X, S), game_valid(X)), Xs),
	maplist(game_id, Xs, Scores),
	sum_list(Scores, Score).

solve_puzzle_b(Score) :-
	load_data(S),
	maplist(game_min_cubes, S, Scores),
	sum_list(Scores, Score).