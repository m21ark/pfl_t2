:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(between)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(system)).



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

abs(X, Y):- X >= 0, Y is X.
abs(X, Y):- X < 0, Y is -X.


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

player(human).
player(computer).

phase(drop).
phase(capture).
phase(peek).

play:-
	repeat,
	game_menu_show,
	read_until_between(1,3, OPT),
	switch(OPT, [
		1: display_game(human-human-0),
		2: display_level_pc(human-computer),
		3: display_level_pc(computer-computer)
	]),
	nl, write('End of game.'), nl, nl, fail.

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


% ======================= GAME LOGIC =======================

% TODO... a especificação do stor pede um argumento size a passar nesta função
initial_state(Size, Board-WhiteTurn-WhiteCount-BlackCount):-
	Board = [['O','O','W','O','O'],
			 ['O','O','W','O','O'],
			 ['O','O','O','O','O'],
			 ['B','B','W','B','B'],
			 ['O','O','B','O','O'],
			 ['O','O','O','O','O']],
	WhiteTurn = 1,
	WhiteCount = 0,
	BlackCount = 0.



choose_move(GameState, Player-Phase, Level, Move) :-

	(
		(Level == 1; Phase=peek) -> 
			valid_moves(GameState, Player-Phase, Moves),
			random_member(Move, Moves)
	);

	(
		Level == 2 ->
			get_best_play(GameState-Player, Player-Phase, Move),
			write('Best play is : '),nl,
			write(Move)
	).



test:-
	Board = [['O','O','W','O','O'],
			 ['O','O','W','O','O'],
			 ['O','O','O','O','O'],
			 ['B','B','W','B','B'],
			 ['O','O','B','O','O'],
			 ['O','O','O','O','O']],
	WhiteTurn = 1,
	WhiteCount = 1,
	BlackCount = 1,

	Player = 'W',

	get_best_play(Board-Player, Player-capture, BestPlay),
	nl,nl,nl,write('Best play is : '),nl,
	write(BestPlay),nl,nl,nl,
	board_print(Board),nl,nl,
	move(Board, BestPlay, Player-capture, NewBoard),
	nl,nl,board_print(NewBoard).

%CC-CR/NC-NR
% RESULT = -1 -1 O O
% RESULT = -1 2 O B
%detect_match2(Board, RetC-RetR, ColorC-ColorR),
%format('RESULT = ~d ~d ~w ~w\n', [RetC, RetR, ColorC,ColorR]).

/*
minmax:
* Generate all possible moves from this situation.
* If no such move exists this situation means an ended game, and the active player loses. Return a value (1000/-1000 depending if it was the min or max players turn)
* If possible moves exist, call minmax for the situation of the board after each move.
* When all the subsequent calls returned, chose the one with the highest/lowest score, and return it.
*/

% get_best_play(Board, Player-peek, BestPlay):-choose_move(Board, Player-peek, 1, BestPlay).

get_best_play(Board-ObjP,Player-Phase,BestPlay):-
	write(Phase), nl,
	minmax(Board-ObjP,Player-Phase,BestPlay,3);
	nl,nl,write('Thinking a bit more for this one'),nl,nl,
	minmax(Board-ObjP,Player-Phase,BestPlay,5);
	nl,nl,write('Well, I am a bad looser. Lets just throw a move out of the box.'),nl,nl,
	choose_move(Board, Player-Phase, 1, BestPlay).


minmax(Board-ObjP,Player-Phase,BestSucc, Level) :-    
	minmax(Board-ObjP,Player-Phase,BestSucc,Value,Level),!, %level tem de ser impar para cair na msm cor q começa
	write('Value:'),write(Value),nl,
	if(Value =< -40000, fail, true).


minmax(Board-ObjP,Player-Phase,BestSucc,Value,Depth) :-  
	valid_moves(Board, Player-Phase, MoveList),
	executeAll(Board-ObjP,Player-Phase,MoveList,BestSucc,Value,Depth). 

%===============================================================================


pc_move_avaliator2(Board-ObjP, Color-Phase, Score, CC-CR/NC-NR):-

	(match_(Board, Color, NC-NR, RC-RR),  (RC >= 0; RR >= 0))->
		(
		ObjP == Color, Score is 1000;
		Score is -1050
		)
	;
	Score is 0.


%===============================================================================

executeAll(_-P,'W'-_,[],_, Score,_) :- P == 'W' -> Score is -100000; Score is 100000. %nl,nl,write('here 1'),nl,nl.
executeAll(_-P,'B'-_,[],_, Score,_) :- P == 'B' -> Score is -100000; Score is 100000. %nl,nl,write('here 2'),nl,nl.

% if depth of recursion reaches limit, value is approximated 
executeAll(Board-P,'W'-_,_,_,Score, 0) :-  P == 'W' -> Score is -100000; Score is 100000.
executeAll(Board-P,'B'-_,_,_,Score, 0) :-  P == 'B' -> Score is -100000; Score is 100000.


% executeAll([['O','O','W','O','O'],
% 			 ['O','O','W','O','O'],
% 			 ['O','O','O','O','O'],
% 			 ['B','B','W','B','B'],
% 			 ['O','O','B','O','O'],
% 			 ['O','O','O','O','O']]-'W','W'-capture, [ 2-1/2-2, 2-3/2-2], B, V, 3 ).



% executeAll([['O','O','W','O','O'],
% 			 ['O','O','O','O','O'],
% 			 ['O','O','W','O','O'],
% 			 ['B','B','W','O','B'],
% 			 ['O','O','B','B','O'],
% 			 ['O','O','O','O','O']]-'W','W'-capture, [2-0/2-1,2-2/2-1], B, V, 3 ).

executeAll(Board-ObjP,Player-Phase,[CC-CR/NC-NR|MoveList],BestSucc,Value,Depth) :-  

	move(Board, CC-CR/NC-NR, Player-Phase, NewBoard1),
	pc_move_avaliator2(NewBoard1-ObjP, Player-Phase, Score, CC-CR/NC-NR),
	
	
	if((Score > 0; Score < 0),
		(choose_move(NewBoard1, Player-peek, 1, _-_/SC-SR), set_piece(NewBoard1, SR, SC, 'O', NewBoard)),
		(NewBoard = NewBoard1)
	),

	game_over(NewBoard, Winner), 
	(
		Winner == ObjP,
		Value is 120000, BestSucc = CC-CR/NC-NR,!;

		Winner == Player,  
		Value is -120000, BestSucc = CC-CR/NC-NR,!;

		next_color(Player, NextPlayer),
		D is Depth - 1, 
		minmax(NewBoard-ObjP,NextPlayer-Phase,_,Value1,D),
		executeAll(Board-ObjP,Player-Phase,MoveList,BestSucc2,Value2,Depth),

		% board_print(NewBoard),nl,nl,write('Score: '),write(Score), write('Value:'), write(Value1),nl,nl,
		if(BestSucc2=CC-CR/NC-NR,true,true),  
		ThisValue is Value1 + Score,
		(
		ObjP == Player -> 
			(
				if(
					ThisValue > Value2,
					(Value=ThisValue, BestSucc=CC-CR/NC-NR),
					(Value=Value2, BestSucc=BestSucc2)
				)
				
					%write('EXIT MAIOR: '), write(Value), nl
			);
			(
				if(
					ThisValue < Value2,
					(Value=ThisValue, BestSucc=CC-CR/NC-NR),
					(Value=Value2, BestSucc=BestSucc2)
				)
				%write('EXIT MENOR: '), write(Value), nl
			)
		)
	).
	%write('EXIT BIG: '), write(Value), nl.

	% if(	
	% 	ThisValue > Value2,
	% 	(Value=ThisValue, BestSucc=CC-CR/NC-NR),
	% 	(Value=Value2, BestSucc=BestSucc2)
	% ).


detect_match2(Board, RetC-RetR, ColorC-ColorR):- 
	detect_match_lines2(Board, 0, RetC, ColorC),
	transpose(Board, BoardT),
	detect_match_lines2(BoardT, 0, RetR, ColorR).

detect_match_line2([], 'O'):-!.
detect_match_line2([[C,V]|T], Color):-
	
	(
		(C == 3,V =='O')-> Color = 'O'
	); 
	
	(
		(C == 3,V \='O')-> Color = V
	);
	
	C \= 3,
	detect_match_line2(T, Color).
	
detect_match_lines2([], _, -1, 'O'):-!.
detect_match_lines2([H|T], N, Ret, Color):-
	
	rle(H, L),
	detect_match_line2(L, Color1),
	Color1 \= 'O'->
		Color = Color1, Ret = N;

	N1 is N+1,
	detect_match_lines2(T, N1, Ret, Color).

	

decrement_count(WhiteTurn, WhiteCount-BlackCount, NW-NB) :-
	WhiteTurn == 1 -> NW is WhiteCount-1, NB is BlackCount;
	NW is WhiteCount, NB is BlackCount-1.

drop_phase(Board, _X, 0, _, Board):- _X == -1, !.
drop_phase(Board, 0, _X, _, Board):- _X == -1, !.
drop_phase(Board, 0, 0, _, Board):-!.
drop_phase(Board, -1, -1, _, Board):-!.
drop_phase(Board, WhiteCount, BlackCount, WhiteTurn-Level-[Cplayer,NewP], New_Board):-
	get_color_from_player(WhiteTurn, Color),
	valid_moves(Board, Color-drop, Pmvs),
	length(Pmvs, L),
	next_turn(WhiteTurn, NewT), !, 
	(
	(
		L > 0,
		(
			Cplayer == computer -> % maybe fazer uma função com este pedaço de código
				choose_move(Board, Color-drop, Level, Move),
				move(Board, Move, Color-drop, New_Board1),
				decrement_count(WhiteTurn, WhiteCount-BlackCount, NewW-NewB),
				format('Computer ~w played:\n',[Color]),
				board_print(New_Board1),nl,
				sleep(2),
				write('\33\[2J'),
				drop_phase(New_Board1, NewW, NewB, NewT-Level-[NewP, Cplayer], New_Board);
	
			board_print(Board),
			%format('\nStones placed: w=~d, b=~d\n',[WhiteCount, BlackCount]),
	
			WhiteTurn==1-> 
				% if(whiteTurn and can_set_any('W'))
				piece_drop(Board, 'W', Board_),
				New_WC is WhiteCount-1,
				drop_phase(Board_, New_WC, BlackCount, 0-Level-[NewP, Cplayer], New_Board);
	
			piece_drop(Board, 'B', Board_),
			New_BC is BlackCount-1,
			drop_phase(Board_, WhiteCount, New_BC, 1-Level-[NewP, Cplayer], New_Board)
		)
	);
	( % se não houver jogadas possíveis para o jogador atual então passa a vez
		WhiteTurn == 1 -> 
			drop_phase(Board, -1, BlackCount, NewT-Level-[NewP, Cplayer], New_Board);
		drop_phase(Board, WhiteCount, -1, NewT-Level-[NewP, Cplayer], New_Board)
	)
	).


valid_moves(GameState, Color-capture, Moves):- 
	findall(Move, move(GameState, Move, Color-capture, NewState), Moves).

valid_moves(GameState, Color-drop, Moves):-
	findall(0-0/NC-NR, (get_piece(GameState, NR, NC, 'O'), move(GameState, 0-0/NC-NR, Color-Phase, NB)), Moves).

% mexi aqui ent posso ter feito asneira :)
valid_moves(GameState, Color-peek, Moves):-
	next_color(Color, NColor),
	findall(0-0/NC-NR, get_piece(GameState, NR, NC, NColor), Moves);
	findall(Move, move(GameState, Move, Color-Phase, NewState), Moves).


% TODO ... make this a failure driven loop
capture_phase(Board, WhiteTurn-Level-[Cplayer,NewP], New_Board):-
	game_over(Board, Winner),
	(
	(
		Winner \= 'O' -> New_Board = Board
	);
	Cplayer == computer -> 
		get_color_from_player(WhiteTurn, Color),
		choose_move(Board, Color-capture, Level,  CC-CR/NC-NR), 
		move(Board, CC-CR/NC-NR, Color-capture, New_Board1), 
		next_turn(WhiteTurn, NewT),
		(
			(match_(New_Board1, Color, NC-NR, RC-RR), (RC >= 0; RR >= 0) )->
				choose_move(Board, Color-peek, Level, _-_/SC-SR),nl,
				set_piece(New_Board1, SR, SC, 'O', New_Board2),
				format('Computer ~w played:\n',[Color]),
				board_print(New_Board1),nl,
				format('Computer ~w Captured:\n',[Color]),
				board_print(New_Board2),nl,
				
				capture_phase(New_Board2, NewT-Level-[NewP, Cplayer], New_Board);
				format('Computer ~w played:\n',[Color]),
				board_print(New_Board1),nl,
				capture_phase(New_Board1, NewT-Level-[NewP, Cplayer], New_Board)
		);
	(
	board_print(Board),nl,nl,
	(
		WhiteTurn==1 -> (capture_phase_white(Board, Level, [Cplayer,NewP], New_Board));
		capture_phase_black(Board, Level, [Cplayer,NewP], New_Board)
	)
	)).


% MUITO POUCO EFICIENTE .... falta ainda saber como ver a linha/coluna pois só conseguimos saber de um
match_(BoardBefore, C, Col-Row, RC-RR) :-
	detect_match(BoardBefore, RC-RR, CC-CR, Col-Row),
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
	
	


capture_phase_black(Board, Level, NewP, New_Board):-

	piece_move(Board, 'B', B1, NewCol-NewRow),
	match_(B1, 'B', NewCol-NewRow, RetC-RetR),
	nl,board_print(B1),nl,nl,
	reverse(NewP, NextPlayer),
	(
	(
		nonvar(RetC),
		RetC >= 0,
		format('Black match at Col=~d\n\n',[RetC]),
		capture_piece(B1, 'B', B2),
		capture_phase(B2, 1-Level-NextPlayer, New_Board)
	);
	
	(
		nonvar(RetR),
		(RetR) >= 0,
		format('Black match at Row=~d\n\n',[RetR]),
		capture_piece(B1, 'B', B2),
		capture_phase(B2, 1-Level-NextPlayer, New_Board)
	);
		capture_phase(B1, 1-Level-NextPlayer, New_Board)
	).
	
	
capture_phase_white(Board, Level, NewP, New_Board):-

	piece_move(Board, 'W', B1, NewCol-NewRow),
	match_(B1, 'W', NewCol-NewRow, RetC-RetR),
	nl,board_print(B1),nl,nl,
	reverse(NewP, NextPlayer),
	(
	(
		nonvar(RetC),
		RetC >= 0,
		format('White match at Col=~d\n\n',[RetC]),
		capture_piece(B1, 'W', B2),
		capture_phase(B2, 0-Level-NextPlayer, New_Board)
	);
	
	(
		nonvar(RetR),
		(RetR) >= 0,
		format('White match at Row=~d\n\n',[RetR]),
		capture_piece(B1, 'W', B2),
		capture_phase(B2, 0-Level-NextPlayer, New_Board)
	);
		capture_phase(B1, 0-Level-NextPlayer, New_Board)
	).


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


	
capture_piece(Board, Color, New_Board):-
	ask_pos('Take piece at ', Color, Row-Col),
	get_piece(Board, Row, Col, Piece),
	Piece \= 'O', Piece \= Color,
	set_piece(Board, Row, Col, 'O', New_Board).
	
piece_drop(Board, Color, New_Board):-
	ask_pos('Drop piece at ', Color, Row-Col),
	check_cross(Board, Row, Col, Color),
	set_piece(Board, Row, Col, Color, New_Board).
	

% ====================== CHECK DROP PIECE CROSS PATTERN ======================
	
check_cross(Board, Row, Col, Color):-
	(
	get_piece(Board, Row, Col, Pos),
	Pos == 'O'
	), !, 
	(
	Col1 is Col-1,
	Col1 >= 0->
		get_piece(Board, Row, Col1, Pos1),
		Pos1 \= Color;
	true
	), !,
	(
	Col2 is Col+1,
	Col2 =< 4->
		get_piece(Board, Row, Col2, Pos2),
		Pos2 \= Color;
	true
	), !,
	(
	Row1 is Row-1,
	Row1 >= 0->
		get_piece(Board, Row1, Col, Pos3),
		Pos3 \= Color;
	true
	), !,
	(
	Row2 is Row+1,
	Row2 =< 5->
		get_piece(Board, Row2, Col, Pos4),
		Pos4 \= Color;
	true
	).
	

can_set_any(Board, Color, Row, Col):-
	check_cross(Board, Row, Col, Color),!.


% ====================== RUN LENGTH ENCODING ======================

rle([], []):-!.
rle([X], [[1,X]]):-!.

rle([X|XT], [[Count, X]|RestEncoded]) :-
    rle(XT, [[SubCount, X]|RestEncoded]),
    succ(SubCount, Count),!.
    
rle([X|XT], [[1, X], [SubCount, Y] | RestEncoded]) :-
    rle(XT, [[SubCount, Y]|RestEncoded]),
    X \= Y,!.

% ====================== DETECT 3 MATCH ======================

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
		!, % green cut to improve performance .. or is it not ??
		C == 3-> Color = V;
		Color == 'O'
	);

	detect_match_line(T, Color).

% ====================== PIECE MOVE ======================

move(Board, CC-CR/NC-NR, Color-Phase, NewBoard) :-
	Phase == capture ->
		get_piece(Board, CR, CC, CurPos),
		CurPos == Color,
		set_piece(Board, CR, CC, 'O', NB),
		get_piece(Board, NR, NC, NPos),
		NPos == 'O',
		set_piece(NB, NR, NC, Color, NewBoard),
		Cdiff is NC - CC, Rdiff is NR - CR,
		abs(Cdiff, Cabs), abs(Rdiff, Rabs),
		Cabs =< 1, Rabs =< 1, Cabs \= Rabs;
	check_cross(Board, NR, NC, Color),
	set_piece(Board, NR, NC, Color, NewBoard).


piece_move(Board, Color, New_Board, NewCol-NewRow):-

	ask_pos('Move from ', Color, CurRow-CurCol),
	get_piece(Board, CurRow, CurCol, CurPos),
	CurPos == Color,
	set_piece(Board, CurRow, CurCol, 'O', NB),

	ask_pos('Move to ', Color, NewRow-NewCol),
	get_piece(Board, NewRow, NewCol, NPos),
	NPos == 'O',
	set_piece(NB, NewRow, NewCol, Color, New_Board), 

	% VALIDATING THE MOVE ... probaly change to member of valid moves
	Cdiff is NewCol - CurCol, Rdiff is NewRow - CurRow,
	abs(Cdiff, Cabs), abs(Rdiff, Rabs),
	Cabs =< 1, Rabs =< 1,
	Cabs \= Rabs.

% ====================== WINNER DETECTION ======================

game_over(Board, Winner):- 

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

% ====================== AUX FUNCS ======================

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

printColorTag(Color):-
	Color == 'W'->
	write('White: ');
	write('Black: ').

get_color_from_player(WhiteTurn, Color):-
	WhiteTurn == 1 -> Color = 'W';
	Color = 'B'.

next_turn(WhiteTurn, NewWhiteTurn):-
	WhiteTurn == 1 -> NewWhiteTurn = 0;
	NewWhiteTurn = 1.

next_color(Color, NewColor):-
	Color == 'W' -> NewColor = 'B';
	NewColor = 'W'.

get_color(WhiteTurn, Color):-
	WhiteTurn == 1 -> 
	Color = 'W';
	Color = 'B'.
	
swap_turn(Bool, New_Bool):-
	Bool == 1 -> 
	New_Bool = 0;
	New_Bool = 1.
	

