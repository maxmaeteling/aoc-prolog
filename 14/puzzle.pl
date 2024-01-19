:- use_module(library(lists), [append/3, reverse/2, nth1/3, sum_list/2, 
							   nth0/3]).
:- use_module(library(apply), [maplist/3, foldl/4]).
:- use_module(library(clpfd)).
:- use_module(library(pure_input), [phrase_from_file/2]).

rotate_clockwise(Ls0, Ls) :-
	transpose(Ls0, Ls1),
	maplist(reverse, Ls1, Ls).

rotate_counterclockwise(Ls0, Ls) :-
	transpose(Ls0, Ls1),
	reverse(Ls1, Ls).

load_data(R0) :-
	phrase_from_file(reflector(R0), "/home/max/projects/prolog/aoc/2023/14/input").

tilt_north(R0, R) :-
	rotate_counterclockwise(R0, R1),
	maplist(sort_segments, R1, R2),
	rotate_clockwise(R2, R).

score(R0, Sol) :-
	rotate_clockwise(R0, R),
	findall(N, (nth1(_, R, Row), nth1(N, Row, 1)), Ns),
	sum_list(Ns, Sol).

tilt_rotate(R0, R) :- tilt_north(R0, R1), rotate_clockwise(R1, R).

spin_cycle(R0, R) :-
	tilt_rotate(R0, R1),
	tilt_rotate(R1, R2),
	tilt_rotate(R2, R3),
	tilt_rotate(R3, R).

solve_puzzle(Sol) :-
	load_data(R0),
	tilt_north(R0, R1),
	score(R1, Sol).

solve_puzzleb(A, Bl, Sol) :-
	load_data(R0),
	n_spin_cycles(0, 1000, R0, _, [], S0),
	reverse(S0, S),
	append(A, B, S),
	append(Bl, _, B),
	append(Bll, Bll, Bl),
	length(Bll, N),
	N > 1,
	length(A, La),
	F #= (1000000000-La-1) mod N,
	nth0(F, Bll, Sol).

n_spin_cycles(N, N, R, R, S, S).

n_spin_cycles(N0, N, R0, R, S0, S) :-
	N > N0,
	!, spin_cycle(R0, R1),
	!, score(R1, S1),
	N1 is N0+1,
	n_spin_cycles(N1, N, R1, R, [S1|S0], S).

sort_segments(R, S) :-
	parse_segments(R, S0),
	maplist(msort, S0, S1),
	reverse(S1, S2),
	foldl(append, S2, [], S).

reflector([L|Ls]) --> reflector_line(L), "\n", reflector(Ls).
reflector([L]) --> reflector_line(L).

reflector_line([L|Ls]) --> field(L), reflector_line(Ls).
reflector_line([L]) --> field(L).

field(0) --> "#".
field(1) --> "O".
field(2) --> ".".

inner_segment([S|Ss]) --> [S], { S #> 0 }, !, inner_segment(Ss).
inner_segment([]) --> [].

segment([0|S]) --> [0], inner_segment(S).

segments([S|Ss]) --> segment(S), segments(Ss).
segments([S]) --> segment(S).

parse_segments(L, [SF|SR]) :-
	phrase(segments([[_|SF]|SR]), [0|L]).