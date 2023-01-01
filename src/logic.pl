% ======================= GAME LOGIC =======================

initial_state(Col-Row, Board-WhiteTurn-WhiteCount-BlackCount):-
	gen_2d_array(Row, Col, 'O', Board),
	WhiteTurn = 1,
	WhiteCount = 12,
	BlackCount = 12.


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
				choose_move(Board, Color-drop, Level, Move),
				move(Board, Move, Color-drop, New_Board1),
				decrement_count(WhiteTurn, WhiteCount-BlackCount, NewW-NewB),
                format('\nStones placed: w=~d, b=~d\n',[WhiteCount, BlackCount]),
				format('Computer ~w played:\n',[Color]),
				display_game(New_Board1),nl,
				sleep(2),
				write('\33\[2J'),
				drop_phase(New_Board1, NewW, NewB, NewT-Level-[NewP, Cplayer], New_Board);
	
			display_game(Board),
			format('\nStones placed: w=~d, b=~d\n',[WhiteCount, BlackCount]),
	
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
				write('\33\[2J'),
				format('Computer ~w played:\n',[Color]),
				display_game(New_Board1),nl,
				format('Computer ~w Captured:\n',[Color]),
				display_game(New_Board2),nl,
				sleep(2),
				capture_phase(New_Board2, NewT-Level-[NewP, Cplayer], New_Board);
				write('\33\[2J'),
				format('Computer ~w played:\n',[Color]),
				display_game(New_Board1),nl,
				sleep(2),
				capture_phase(New_Board1, NewT-Level-[NewP, Cplayer], New_Board)
		);
	(
	display_game(Board),nl,nl,
	(
		WhiteTurn==1 -> (capture_phase_white(Board, Level, [Cplayer,NewP], New_Board));
		capture_phase_black(Board, Level, [Cplayer,NewP], New_Board)
	)
	)).


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
	nl,display_game(B1),nl,nl,
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
	nl,display_game(B1),nl,nl,
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

