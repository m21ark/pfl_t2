% This file contains the definitions of game Logic related predicates used in the program.


% ======================= GAME LOGIC =======================

:-dynamic board_size/2.

% initial_state(+Col-Row, -Board-WhiteTurn-WhiteCount-BlackCount)/2
% defines the initial state of the game, with the board (Col-Row given), the turn and the number of pieces left to place.
initial_state(Col-Row, Board-WhiteTurn-WhiteCount-BlackCount):-
	gen_2d_array(Row, Col, 'O', Board),
	asserta((board_size(R, C) :- R is Row, C is Col)),
	WhiteTurn = 1,
	WhiteCount is Col*Row/2 - 3,
	BlackCount is Col*Row/2 - 3.

% drop_phase(+Board, +WhiteCount, +BlackCount, +WhiteTurn-Level-Players, -New_Board)/5
% defines the drop phase of the game, where the players place their pieces on the board.
% Board: the current board
% WhiteCount: the number of white pieces left to place
% BlackCount: the number of black pieces left to place
% WhiteTurn: the current player turn
% Level: the level of the computer player
% Players: the players of the game [Cplayer,NewP]
% New_Board: the new board after the drop phase
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
			Cplayer == computer -> 
				choose_move(Board, Color-drop, Level, _X-_Y/NC-NR),
				move(Board, _X-_Y/NC-NR, Color-drop, New_Board1),
				decrement_count(WhiteTurn, WhiteCount-BlackCount, NewW-NewB),
				clear,
                format('\nStones left to place: w=~d, b=~d\n',[WhiteCount, BlackCount]),
				convert_from_move_to_notation(NC-NR, MRep),	
				format('Computer ~w played ~w:\n',[Color, MRep]),
				display_game(New_Board1),nl,
				sleep(2),
				clear,
				drop_phase(New_Board1, NewW, NewB, NewT-Level-[NewP, Cplayer], New_Board);
	
			display_game(Board),
			format('\nStones left to place: w=~d, b=~d\n',[WhiteCount, BlackCount]),
	
			WhiteTurn==1-> 
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

% capture_phase(+Board, +WhiteTurn-Level-Players, -New_Board)/3
% defines the capture phase of the game, where the players can capture their opponent's pieces with 3 pieces allign.
% Board: the current board
% WhiteTurn: the current player turn
% Level: the level of the computer player
% Players: the players of the game [Cplayer,NewP]
% New_Board: the new board after the capture phase
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
				clear,
				convert_from_move_to_notation(NC-NR, MRep),
				format('Computer ~w played ~w:\n',[Color, MRep]),
				display_game(New_Board1),nl,
				format('Computer ~w Captured:\n',[Color]),
				display_game(New_Board2),nl,
				sleep(2),
				clear,
				capture_phase(New_Board2, NewT-Level-[NewP, Cplayer], New_Board);
			clear,
			convert_from_move_to_notation(NC-NR, MRep),
			format('Computer ~w played ~w:\n',[Color, MRep]),
			display_game(New_Board1),nl,
			sleep(2),
			clear,
			capture_phase(New_Board1, NewT-Level-[NewP, Cplayer], New_Board)
		);
	(
	display_game(Board),nl,nl,
	(
		WhiteTurn==1 -> (capture_phase_white(Board, Level, [Cplayer,NewP], New_Board));
		capture_phase_black(Board, Level, [Cplayer,NewP], New_Board)
	)
	)).

% match_(+Board, +Color, +Col-Row, -RC-RR)/4
% checks if there is a match (horizontally and vertically) of 3 pieces of the same color as Color in the position played.
% Board: the current board
% C: the color of the piece played
% Col-Row: the position played
% RC-RR: the position of the piece that was captured
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
	
	

% capture_phase_black(+Board, +Level, +Players, -New_Board)/4
% defines the capture phase of the game for the black player.
% Board: the current board
% Level: the level of the computer player
% NewP: the next player of the game [Cplayer,NewP]
% New_Board: the new board after the capture phase
capture_phase_black(Board, Level, NewP, New_Board):-

	piece_move(Board, 'B', B1, NewCol-NewRow),
	match_(B1, 'B', NewCol-NewRow, RetC-RetR),
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
	
% capture_phase_white(+Board, +Level, +Players, -New_Board)/4
% defines the capture phase of the game for the white player.
% Board: the current board
% Level: the level of the computer player
% NewP: the next player of the game [Cplayer,NewP]
% New_Board: the new board after the capture phase
capture_phase_white(Board, Level, NewP, New_Board):-

	piece_move(Board, 'W', B1, NewCol-NewRow),
	match_(B1, 'W', NewCol-NewRow, RetC-RetR),
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


% ====================== BOARD LOGIC ======================

% get_piece(+Board, +Row, +Col, -Piece)/4
% gets the piece in the position Row-Col of the board.
% Board: the current board
% Row: the row of the piece
% Col: the column of the piece
% Piece: the piece in the position Row-Col
get_piece(Board, Row, Col, Piece):-
	nth0(Row, Board, Row_),
	nth0(Col, Row_, Piece).

% set_piece(+Board, +Row, +Col, +Color, -New_Board)/5
% sets the piece in the position Row-Col of the board to Color.
% Board: the current board
% Row: the row of the piece
% Col: the column of the piece
% Color: the color of the piece
% New_Board: the new board after the piece is set
set_piece(Board, Row, Col, Color, New_Board):-
	nth0(Row, Board , Row_),
	list_replace(Row_, Col, Color, New_Row),
	list_replace(Board, Row ,New_Row ,New_Board).

% capture_piece(+Board, +Color, -New_Board)/3
% captures a piece of the opposite color, from position asked to the user.
% Board: the current board
% Color: the color of the piece that is capturing
% New_Board: the new board after the piece is captured
capture_piece(Board, Color, New_Board):-
	ask_pos('Take piece at ', Color, Row-Col),
	get_piece(Board, Row, Col, Piece),
	Piece \= 'O', Piece \= Color,
	set_piece(Board, Row, Col, 'O', New_Board).

% piece_drop(+Board, +Color, -New_Board)/3
% drops a piece of the same color in a position asked to the user.
% Board: the current board
% Color: the color of the piece that is dropping
% New_Board: the new board after the piece is dropped
piece_drop(Board, Color, New_Board):-
	ask_pos('Drop piece at ', Color, Row-Col),
	check_cross(Board, Row, Col, Color),
	set_piece(Board, Row, Col, Color, New_Board).

% move(+Board, ?Move, +Color-Phase, -New_Board)/4
% moves a piece, legally, of the same color from a position to another.
% Board: the current board
% Move: the move to be made
% Color-Phase: the color and phase of the move
% New_Board: the new board after the piece is moved
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
	

% ====================== CHECK DROP PIECE CROSS PATTERN ======================

% check_cross(+Board, +Row, +Col, +Color)/4
% checks if the piece in position Row-Col of the board is in a cross pattern.
% Board: the current board
% Row: the row of the piece
% Col: the column of the piece
% Color: the color of the piece
check_cross(Board, Row, Col, Color):-
	board_size(MR, MC),
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
	Col2 =< MC-1 ->
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
	Row2 =< MR-1->
		get_piece(Board, Row2, Col, Pos4),
		Pos4 \= Color;
	true
	).
	

% ====================== DETECT 3 MATCH ======================

% detect_match(+Board, -RetC-RetR, -ColorC-ColorR, +Col-Row)/4
% detects if there is a match of 3, horizontal and vertically , pieces in the board at Col-Row.
% Board: the current board
% RetC-RetR: the column and row of the match
% ColorC-ColorR: the color of the match
% Col-Row: the column and row of the piece that is being dropped
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

% detect_match_line(+List, -Color)/2
% detects if there is a match of 3 pieces in the line.
% List: the list of the line
% Color: the color of the match to check against
detect_match_line([], 'O'):-!.
detect_match_line([[C,V]|T], Color):-
	(
		!, % green cut to improve performance .. or is it not ??
		C == 3-> Color = V;
		Color == 'O'
	);

	detect_match_line(T, Color).

% ====================== PIECE MOVE ======================

% piece_move(+Board, +Color, -New_Board, -NewCol-NewRow)/4
% moves a piece of the same color from a position to another, asking the input to the user.
% Board: the current board
% Color: the color of the piece that is moving
% New_Board: the new board after the piece is moved
% NewCol-NewRow: the new column and row of the piece
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

% game_over(+Board, -Winner)/2
% detects if there is a winner in the game.
% Board: the current board
% Winner: the winner of the game
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

