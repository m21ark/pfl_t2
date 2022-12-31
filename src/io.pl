% ======================= INPUT STUFF =======================

read_number_acc(Acc, Acc) :- peek_code(10), !.

read_number_acc(Acc, Ret) :- 
	\+ peek_code(10),
	get_code(C),
	char_code('0', Zero),
	D is C-Zero,
	D >= 0, D < 10,
	Nacc is Acc*10 + D,
	read_number_acc(Nacc, Ret).

read_number(X) :- 
	read_number_acc(0, X),
	get_code(10).


read_until_between(Min, Max, Ret):-
	write('> '),read_number(V),
	between(Min,Max, V) -> Ret is V;
	write('Invalid number!'),nl,
	read_until_between(Min, Max, Ret).

read_string("") :- peek_code(10),!,get_code(_).
read_string([C | T]) :- get_code(C), read_string(T).

read_char(C):-get_char(C), get_char(_).


% ======================= PRINT STUFF =======================


print_n(_ , 0):-!.
print_n(S, N):-
	N1 is N-1,
	write(S),
	print_n(S, N1).
	
% b)

print_text(T, S, P):-
	write(S),
	print_n(' ', P),
	write(T),
	print_n(' ', P),
	write(S).

% c)

print_Vpadd(_, _, 0):-!.
print_Vpadd(S, L, N):-
	write(S),
	print_n(' ', L) ,
	write(S),nl,
	N1 is N-1,
	print_Vpadd(S, L, N1).
	

% ======================= MENU DISPLAY =======================

game_menu_show:- % (T, S, P, V)
	
	atom_chars('Wali Game', K), % string to list
	length(K, L),
	L2 is L + 2*10,
	L3 is L2 + 2,
	
	nl,print_n('*', L3), nl,
	print_Vpadd('*', L2, 1),	
	print_text('Wali Game', '*' ,10), nl,
	print_Vpadd('*', L2, 1),	
	print_n('*', L3), nl,
	print_Vpadd('*', L2, 1),	
	print_text('1) Human vs Human      ', '*' ,3), nl,
	print_Vpadd('*', L2, 1),	
	print_text('2) Human vs PC         ', '*' ,3), nl,
	print_Vpadd('*', L2, 1),	
	print_text('3) PC vs PC            ', '*' ,3), nl,
	print_Vpadd('*', L2, 1),	
	print_n('*', L3), nl.


pc_menu_level_show:-
	
	atom_chars('Wali Game', K), % string to list
	length(K, L),
	L2 is L + 2*10,
	L3 is L2 + 2,
	
	nl,print_n('*', L3), nl,
	print_Vpadd('*', L2, 1),	
	print_text('AI Level ', '*' ,10), nl,
	print_Vpadd('*', L2, 1),	
	print_n('*', L3), nl,
	print_Vpadd('*', L2, 1),	
	print_text(' 1) Random             ', '*' ,3), nl,
	print_Vpadd('*', L2, 1),	
	print_text(' 2) Inteligent         ', '*' ,3), nl,
	print_Vpadd('*', L2, 1),	
	print_n('*', L3), nl.

% ======================= GAME DISPLAY =======================

display_level_pc(P1-P2):-
	pc_menu_level_show,
	read_until_between(1,2, PC_Level), 
	display_game(P1-P2-PC_Level).

display_game(P1-P2-PC_Level) :- 
	initial_state(6, Board-WhiteTurn-WhiteCount-BlackCount),
	random_permutation([P1, P2], Turns),
	drop_phase(Board, WhiteCount, BlackCount, 1-PC_Level-Turns, NB),
	capture_phase(NB, 1-PC_Level-Turns, New_Board), 
	game_over(New_Board, Winner),
	board_print(New_Board),
	format('The winner is: ~w', [Winner]), ! .	

% ====================== BOARD PRINT ======================

board_print(Board):- 
	write('\n '), 
	board_print_(Board, -1), 
	print_n('-', 14),
	write('\n a  b  c  d  e\n').

board_print_([], _):-!.
board_print_([H|T], N):- 
	
	\+ isList(H) -> 
	write(H), 
	write('  '),
	board_print_(T,N1);
	
	N1 is N+1,
	board_print_(H,N1),
	write('| '),write(N1),write('\n '), 
	board_print_(T,N1).
