:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pure_input)).
:- use_module(library(clpfd)).

lines([L|Ls]) --> string_without("\n", L), "\n", lines(Ls).
lines([L]) --> string_without("\n", L).
lines([]) --> [].

string_without_number(S) --> string_without("0123456789", S).

find_number([Offs, Num], Offs0) -->
	string_without_number(S),
	digits(Num),
	{ length(S, L0), Offs #= Offs0+L0 }.

find_numbers([[Offs, Num]|Ns], Offs0) -->
	find_number([Offs, Num0], Offs0),
	{ number_string(Num, Num0), length(Num0, L0), Offs1 #= Offs+L0 },
	find_numbers(Ns, Offs1).
find_numbers([], _) --> string_without_number(_).

parse_numbers(L, N) :- phrase(find_numbers(N, -1), [` `|L]).

number_valid(Ns, R0, C0, N, Fn, R, C) :-
	number_chars(N, S),
	length(S, L),
	RF #= R0-1,
	RT #= R0+1,
	CF #= C0-1,
	CT #= C0+L,
	between(RF, RT, R),
	between(CF, CT, C),
	nth0(R, Ns, Row),
	nth0(C, Row, Char),
	call(Fn, Char).

is_symb(C) :- string_codes("#$%&*+-/=@", Cs), memberchk(C, Cs).
is_gear(0'*).

load_data(L, Ns) :-
	phrase_from_file(lines(L), '/home/max/projects/prolog/aoc/2023/03/input'),
	maplist(parse_numbers, L, Ns).

solve_puzzle(S) :-
	load_data(L, Ns),
	findall(X,
			(nth0(R, Ns, Row),
			 nth0(_, Row, [C, X]),
			 number_valid(L, R, C, X, is_symb, _, _)),
			Xs0),
	sum_list(Xs0, S).

gear_ratio(Xs, R, C, N) :-
	findall(G, nth0(_, Xs, [_, _, G, R, C]), Gs),
	length(Gs, 2),
	nth0(0, Gs, N0),
	nth0(1, Gs, N1),
	N #= N0*N1.

solve_puzzleb(X) :-
	load_data(L, Ns),
	findall([R0, C0, X0, RG, CG],
			(nth0(R0, Ns, Row),
			 nth0(_, Row, [C0, X0]),
			 number_valid(L, R0, C0, X0, is_gear, RG, CG)),
			Xs),
	findall(N, (nth0(_, Xs, [_, _, _, R1, C1]),
				gear_ratio(Xs, R1, C1, N)), Ns0),
	sum_list(Ns0, X0),
	X is X0/2.
