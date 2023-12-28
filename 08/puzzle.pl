:- use_module(library(dcg/basics), [string_without/4]).
:- use_module(library(clpfd)).
:- use_module(library(dif), [dif/2]).
:- use_module(library(lists), [nth0/3, reverse/2]).
:- use_module(library(pure_input), [phrase_from_file/2]).
:- use_module(library(apply), [maplist/3, maplist/2, maplist/4, 
							   foldl/4]).

dir(r) --> "R".
dir(l) --> "L".

dirs([D|Ds]) --> dir(D), dirs(Ds).
dirs([]) --> [].

line(From, L, R) -->
	string_without(" ", From),
	" = (",
	string_without(",", L),
	", ",
	string_without(")", R), ")".

nodes([n(F, L, R)|Ls]) --> line(F, L, R), "\n", nodes(Ls).
nodes([n(F, L, R)]) --> line(F, L, R).

network(Seq, Nodes) --> dirs(Seq), "\n\n", nodes(Nodes).

traverse([], Seq, N, Nodes, I0, I) :-
	traverse(Seq, Seq, N, Nodes, I0, I).

traverse([_|_], _, `ZZZ`, _, I, I).

traverse([l|Ss], Seq, N0, Nodes, I0, I) :-
	dif(N0, `ZZZ`),
	nth0(_, Nodes, n(N0, N, _)),
	I1 #= I0+1,
	traverse(Ss, Seq, N, Nodes, I1, I).

traverse([r|Ss], Seq, N0, Nodes, I0, I) :-
	dif(N0, `ZZZ`),
	nth0(_, Nodes, n(N0, _, N)),
	I1 #= I0+1,
	traverse(Ss, Seq, N, Nodes, I1, I).

solve_puzzle(I) :-
	phrase_from_file(network(S, N), "/home/max/projects/prolog/aoc/2023/08/input"),
	traverse(S, S, `AAA`, N, 0, I).

is_begin(N0) :- char_code('A', C), reverse(N0, [C|_]).
is_end(N0) :- char_code('Z', C), reverse(N0, [C|_]).

next_left_nodes(Ns, N0, NL) :-
	nth0(_, Ns, n(N0, NL, _)).

next_right_nodes(Ns, N0, NR) :-
	nth0(_, Ns, n(N0, _, NR)).

traverse_ghost_next(_, _, _, N, N, I, I) :-
	 maplist(is_end, N).

traverse_ghost_next(Seq0, Seq, Nodes, N1, N, I0, I) :-
	\+ maplist(is_end, N1),
	I1 #= I0+1,
	traverse_ghost(Seq0, Seq, Nodes, N1, N, I1, I).

traverse_ghost([], Seq, Nodes, N0, N, I0, I) :-
	traverse_ghost(Seq, Seq, Nodes, N0, N, I0, I).

traverse_ghost([l|Ss], Seq, Nodes, N0, N, I0, I) :-
	maplist(next_left_nodes(Nodes), N0, N1),
	traverse_ghost_next(Ss, Seq, Nodes, N1, N, I0, I).

traverse_ghost([r|Ss], Seq, Nodes, N0, N, I0, I) :-
	maplist(next_right_nodes(Nodes), N0, N1),
	traverse_ghost_next(Ss, Seq, Nodes, N1, N, I0, I).

traverse_ghost_one(Seq, Nodes, N0, N, I) :-
	traverse_ghost(Seq, Seq, Nodes, [N0], [N], 1, I).

lcm_(A, B, C) :- C is lcm(A, B).

solve_puzzleb(B) :-
	phrase_from_file(network(S, N), "/home/max/projects/prolog/aoc/2023/08/input"),
	findall(X, (nth0(_, N, n(X, _, _)), is_begin(X)), Xs),
	maplist(traverse_ghost_one(S, N), Xs, _, Is),
	foldl(lcm_, Is, 1, B).