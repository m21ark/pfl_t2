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
	
decrement_count(WhiteTurn, WhiteCount-BlackCount, NW-NB) :-
	WhiteTurn == 1 -> NW is WhiteCount-1, NB is BlackCount;
	NW is WhiteCount, NB is BlackCount-1.