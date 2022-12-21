:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(between)).
:- use_module(library(clpfd)).

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
	
flatten([], []).
flatten([H|T], Flat) :-
	isList(H), !,
	flatten(H, NewH),
	flatten(T, NewT),
	append(NewH, NewT, Flat).
flatten([H|T], [H|Flat]) :-
	flatten(T, Flat).

succ(X, Y):- Y is X+1.


diff([], [], []).
diff([H1|T1], [H2|T2], [H1|Diff]) :- H1 \= H2, diff(T1, T2, Diff).
diff([H1|T1], [H2|T2], Diff) :- H1 = H2, diff(T1, T2, Diff).

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
insert_elem(_, [], E, Ret):- Ret = [E],!.
insert_elem(I, [H|T], E, Ret):-
	I > 0, 
	I1 is I-1,
	insert_elem(I1, T, E, L),
	append([H], L, Ret).
  
delete_elem(0, [_|T], Ret):- append([], T, Ret), !.
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
	 
% whiteTurn = true. 
% whiteCount = 12.
% blackCount = 12.	 


% nb_setval(name, value) and nb_getval(name, value).

main:-
	repeat,
	game_menu_show,
	read_until_between(1,3, OPT),
	
	switch(OPT, [
		1: play,
		2: write('option 2'),
		3: write('option 3')
	]),

	nl, write('End of program.'), fail.
	
% TODO... a especificação do stor pede um argumento size a passar nesta função
initial_state(Board-WhiteTurn-WhiteCount-BlackCount):-
	Board = [['O','O','O','O','O'],
			['O','O','O','O','O'],
			['O','O','O','O','O'],
			['O','O','O','O','O'],
			['O','O','O','O','O'],
			['O','O','O','O','O']],
	WhiteTurn = 1,
	WhiteCount = 12,
	BlackCount = 12.

play:-

	initial_state(Board-WhiteTurn-WhiteCount-BlackCount),

	drop_phase(Board, 12, 12, 1, New_Board),
	capture_phase(Board, 1, New_Board),
	check_if_winner(New_Board, Winner),
	board_print(New_Board),
	format('The winner is: ~w', [Winner]), ! .
	

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




capture_phase(Board, WhiteTurn, New_Board):-

	check_if_winner(Board, Winner),
	(
		Winner \= 'O' -> New_Board = Board
	);
	
	board_print(Board),nl,nl,
	
	(
		WhiteTurn==1,
		capture_phase_white(Board, New_Board)
	);
	
	WhiteTurn==0,
	capture_phase_black(Board, New_Board).


% MUITO POUCO EFICIENTE .... falta ainda saber como ver a linha/coluna pois só conseguimos saber de um
match_(BoardBefore, C, Col-Row, RC-RR) :-
	detect_match(BoardBefore, RC-RR, CC-CR, Col-Row),
	write(CC-CR), nl,
	write(Col-Row),
	(
		CC == C;
		CR == C
	),
	(
		RC \= -1 -> 
			REnd is RC + 2,
			Col >= RC,
			Col =< REnd
		;
			REnd is RR + 2,
			Row >= RR,
			Row =< REnd
	);

	RC is -1,
	RR is -1.
	
	


capture_phase_black(Board, New_Board):-

	piece_move(Board, 'B', B1, NewCol-NewRow),
	match_(B1, 'B', NewCol-NewRow, RetC-RetR),
	nl,board_print(B1),nl,nl,
	(
	(
		nonvar(RetC),
		RetC >= 0,
		format('Black match at Col=~d\n\n',[RetC]),
		capture_piece(B1, 'B', B2),
		capture_phase(B2, 1, New_Board)
	);
	
	(
		nonvar(RetR),
		(RetR) >= 0,
		format('Black match at Row=~d\n\n',[RetR]),
		capture_piece(B1, 'B', B2),
		capture_phase(B2, 1, New_Board)
	);
	
	capture_phase(B1, 1, New_Board)
	);
	
	write('You shouldnt come here...2').
	
	
capture_phase_white(Board, New_Board):-

	piece_move(Board, 'W', B1, NewCol-NewRow),
	match_(B1, 'W', NewCol-NewRow, RetC-RetR),
	nl,board_print(B1),nl,nl,
	(
	(
		nonvar(RetC),
		RetC >= 0,
		format('White match at Col=~d\n\n',[RetC]),
		capture_piece(B1, 'W', B2),
		capture_phase(B2, 0, New_Board)
	);
	
	(
		nonvar(RetR),
		(RetR) >= 0,
		format('White match at Row=~d\n\n',[RetR]),
		capture_piece(B1, 'W', B2),
		capture_phase(B2, 0, New_Board)
	);

	capture_phase(B1, 0, New_Board)
	);
	
	write('You shouldnt come here...1').


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


% ====================== BOARD LOGIC ======================

get_piece(Board, Row, Col, Piece):-
	nth0(Row, Board, Row_),
	nth0(Col, Row_, Piece).

set_piece(Board, Row, Col, Color, New_Board):-
	nth0(Row, Board , Row_),
	list_replace(Row_, Col, Color, New_Row),
	list_replace(Board, Row ,New_Row ,New_Board).


printColorTag(Color):-
	Color == 'W'->
	write('White: ');
	write('Black: ').

ask_pos(Str, Color, Row-Col) :-
	printColorTag(Color),
	write(Str),
	!, repeat, 
	read_string(L),
	length(L, Len),
	nth0(0, L, C),
	nth0(1, L, R),
	Len == 2,
	Col is C - 97,
	Row is R - 48,
	Row >= 0, Row =< 5,
	Col >= 0, Col =< 4,true.
	
capture_piece(Board, Color, New_Board):-
	ask_pos('Take piece at ', Color, Row-Col),
	get_piece(Board, Row, Col, Piece),
	Piece \= 'O', Piece \= Color,
	set_piece(Board, Row, Col, 'O', New_Board).
	
piece_drop(Board, Color, New_Board):-
	ask_pos('Drop piece at ', Color, Row-Col),
	check_cross(Board, Row, Col, Color),
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
	
	
piece_move(Board, Color, New_Board, NewCol-NewRow):-

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


rle([], []):-!.
rle([X], [[1,X]]):-!.

rle([X|XT], [[Count, X]|RestEncoded]) :-
    rle(XT, [[SubCount, X]|RestEncoded]),
    succ(SubCount, Count),!.
    
rle([X|XT], [[1, X], [SubCount, Y] | RestEncoded]) :-
    rle(XT, [[SubCount, Y]|RestEncoded]),
    X \= Y,!.



detect_match(Board, RetC-RetR, ColorC-ColorR, Col-Row):- 
	nth0(Row, Board, RRow),
	rle(RRow, L),
	detect_match_line(L, ColorC),
	(
	ColorC \= 'O' -> RetR is Row;
	RetR is -1
	),
	transpose(Board, BoardT),
	nth0(Col, BoardT, RCol),
	rle(RCol, L2),
	detect_match_line(L2, ColorR),
	(
	ColorR \= 'O' -> RetC is Col;
	RetC is -1
	).

detect_match_line([], 'O'):-!.
detect_match_line([[C,V]|T], Color):-
	
	(
		(C == 3,V =='O')-> Color = 'O'
	); 
	
	(
		(C == 3,V \='O')-> Color = V
	);
	
	C \= 3,
	detect_match_line(T, Color).

