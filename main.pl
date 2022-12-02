:- use_module(library(lists)).
% :- use_module(library(apply)).


% ======================= USEFUL FUNCS =======================

switch(X, [Val:Goal|Cases]):-
	(X = Val -> 
	call(Goal);
	switch(X, Cases)).
	
	
for_loop(N, Func):-for_loop_(N, Func); true.	
for_loop_(N, Func):-
	between(0, N, _),
	call(Func),
	fail.
	
	
% ======================= LIST STUFF =======================

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

isList([_|_]).
isList([]).


countElem_([], _, Acc, Acc):-!.
countElem_([H|T], E, Acc, Ret):-
	H==E-> 
		Acc1 is Acc+1,
		countElem_(T, E, Acc1, Ret);
	countElem_(T, E, Acc, Ret).
	
countElem(L, E, Count):-countElem_(L, E, 0, Count).


insert_elem(0, [H|T], E, Ret):- append([E,H], T, Ret), !.
insert_elem(I, [], E, Ret):- Ret = [E],!.
insert_elem(I, [H|T], E, Ret):-
	I > 0, 
	I1 is I-1,
	insert_elem(I1, T, E, L),
	append([H], L, Ret).
  
delete_elem(0, [H|T], Ret):- append([], T, Ret), !.
delete_elem(I, [H|T], Ret):-
	I > 0, 
	I1 is I-1,
	delete_elem(I1, T, L),
	append([H], L, Ret).

list_replace(List, Index, Value, New_List):-
	delete_elem(Index, List, List_1),
	insert_elem(Index, List_1, Value, New_List).
	

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
	

% ======================= MENU STUFF =======================

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

% ======================= Cenas do stor =======================

/*
play_game:-
	initial_state(GameState-Player),
	display_game(GameState-Player),
	game_cycle(GameState-Player).
	
	
	
game_cycle(GameState-Player):-
	game_over(GameState, Winner), !,
	congratulate(Winner).
	
	
game_cycle(GameState-Player):-
	choose_move(GameState, Player, Move),
	move(GameState, Move, NewGameState),
	next_player(Player, NextPlayer),
	display_game(GameState-NextPlayer), !,
	game_cycle(NewGameState-NextPlayer).


choose_move(GameState, human, Move):-true.
	% interaction to select move
	
	
choose_move(GameState, computer-Level, Move):-
	valid_moves(GameState, Moves),
	choose_move(Level, GameState, Moves, Move).
	
	
valid_moves(GameState, Moves):-
	findall(Move, move(GameState, Move, NewState), Moves).
	
	
choose_move(1, _GameState, Moves, Move):-
	random_select(Move, Moves, _Rest).
	
	
choose_move(2, GameState, Moves, Move):-
	% evaluate_board assumes lower value is better
	setof(Value-Mv, NewState^( member(Mv, Moves),
	move(GameState, Mv, NewState),
	evaluate_board(NewState, Value) ), [_V-Move|_]).
*/


% ======================= MAIN GAME =======================

/*
	[['O','O','O','O','O'],
	 ['O','O','O','O','O'],
	 ['O','O','O','O','O'],
	 ['O','O','O','O','O'],
	 ['O','O','O','O','O'],
	 ['O','O','O','O','O']].
*/
	 
whiteTurn = true. 
whiteCount = 12.
blackCount = 12.	 


% nb_setval(name, value) and nb_getval(name, value).

main:-
	game_menu_show,
	read_until_between(1,3, OPT),
	
	switch(OPT, [
		1: play,
		2: write('option 2'),
		3: write('option 3')
	]),

	nl, write('End of program.').
	


play:-

	Board = [['B','W','B','W','B'],
			 ['O','B','O','B','O'],
			 ['W','O','W','O','W'],
			 ['B','W','O','W','O'],
			 ['W','O','B','O','B'],
			 ['O','B','W','B','W']],

	% drop_phase(Board, 12, 12, 1, New_Board).
	
	board_print(Board),
	C = can_set_any(Board, 'B', Row, Col),
	C->
		format('Space at: Row=~d , Col=~d',[Row, Col]);
	true.
	
	
drop_phase(Board, _, 0, _, Board):-!.
drop_phase(Board, 0, _, _, Board):-!.
drop_phase(Board, WhiteCount, BlackCount, WhiteTurn, New_Board):-

	board_print(Board),
	format('\nStones left to place: w=~d, b=~d\n',[WhiteCount, BlackCount]),
	
	WhiteTurn==1-> 
		% if(whiteTurn and can_set_any('W'))
		piece_drop(Board, 'W', Board_),
		New_WC is WhiteCount-1,
		drop_phase(Board_, New_WC, BlackCount, 0, New_Board);

	piece_drop(Board, 'B', Board_),
	New_BC is BlackCount-1,
	drop_phase(Board_, WhiteCount, New_BC, 1, New_Board).




capture_phase:-true.
	



% ====================== BOARD PRINT ======================

board_print(Board):- 
	write('\n '), 
	board_print_(Board, -1), 
	print_n('-', 14),
	write('\n 0  1  2  3  4\n').

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


% ====================== BOARD LOGIC ======================

get_piece(Board, Row, Col, Piece):-
	nth0(Col, Board, Row_),
	nth0(Row, Row_, Piece).

set_piece(Board, Row, Col, Color, New_Board):-
	nth0(Col, Board , Row_),
	list_replace(Row_, Row, Color, New_Row),
	list_replace(Board, Col ,New_Row ,New_Board).

% TO-DO FUNCS:	
	% def detect_match()

printColorTag(Color):-
	Color == 'W'->
	write('White: ');
	write('Black: ').

% ask_pos('Position desired ', Pos).	
ask_pos(Str, Color, Row-Col):-
	printColorTag(Color),
	write(Str),
	read_until_between(0, 4, Row),
	get_char(_),
	read_until_between(0, 5, Col).
	%format('\nThe values given were:\nrow=~d\ncol=~d\n',[Row, Col]).
	
capture_piece(Board, Color, New_Board):-
	ask_pos('Take piece at ', Color, Row-Col),
	get_piece(Board, Row, Col, Piece),
	Piece \= 'O', Piece \= Color,
	set_piece(Board, Row, Col, 'O', New_Board).
	
	
piece_drop(Board, Color, New_Board):-
	ask_pos('Drop piece at ', Color, Row-Col),
	
	% call check_cross
	% check if Pos is empty
	% white/blackCount-- 
	set_piece(Board, Row, Col, Color, New_Board).
	

get_color(WhiteTurn, Color):-
	WhiteTurn == 1 -> 
	Color = 'W';
	Color = 'B'.
	
swap_turn(Bool, New_Bool):-
	Bool == 1 -> 
	New_Bool = 0;
	New_Bool = 1.
	
	
piece_move(Board, Color, New_Board):-

	ask_pos('Move from ', Color, CurRow-CurCol),
	get_piece(Board, CurRow, CurCol, CurPos),
	CurPos == Color,
	set_piece(Board, CurRow, CurCol, 'O', NB),
	
	ask_pos('Move to ', Color, NewRow-NewCol),
	get_piece(Board, NewRow, NewCol, NPos),
	NPos == 'O',
	set_piece(NB, NewRow, NewCol, Color, New_Board).




check_if_winner(Board, Winner):- 

	(	
	flatten(Board, L),
	countElem(L,'W', Wnum),
	Wnum =< 2-> Winner = 'B'
	);
	
	(
	flatten(Board, L),	
	countElem(L,'B', Bnum),
	Bnum =< 2-> Winner = 'W'
	);
	
	Winner = 'O'.

	
check_cross(Board, Row, Col, Color):-
	(
	get_piece(Board, Row, Col, Pos),
	Pos == 'O'
	),
	(
	Col1 is Col-1,
	Col1 >= 0->
		get_piece(Board, Row, Col1, Pos1),
		Pos1 \= Color;
	true
	),
	(
	Col2 is Col+1,
	Col2 =< 5->
		get_piece(Board, Row, Col2, Pos2),
		Pos2 \= Color;
	true
	),
	(
	Row1 is Row-1,
	Row1 >= 0->
		get_piece(Board, Row1, Col, Pos3),
		Pos3 \= Color;
	true
	),
	(
	Row2 is Row+1,
	Row2 =< 4->
		get_piece(Board, Row2, Col, Pos4),
		Pos4 \= Color;
	true
	).
	

can_set_any(Board, Color, Row, Col):-
	check_cross(Board, Row, Col, Color),!.



