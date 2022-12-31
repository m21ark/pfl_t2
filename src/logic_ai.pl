
% AI 
choose_move(GameState, Player-Phase, Level, Move) :-

	(
		(Level == 1; Phase=peek; Phase=drop) -> 
			valid_moves(GameState, Player-Phase, Moves),
			random_member(Move, Moves)
	);

	(
		Level == 2 ->
			get_best_play(GameState-Player, Player-Phase, Move),
			write('Best play is : '),nl,
			write(Move)
	).

/*
minmax:
* Generate all possible moves from this situation.
* If no such move exists this situation means an ended game, and the active player loses. Return a value (1000/-1000 depending if it was the min or max players turn)
* If possible moves exist, call minmax for the situation of the board after each move.
* When all the subsequent calls returned, chose the one with the highest/lowest score, and return it.
*/

get_best_play(Board-ObjP,Player-Phase,BestPlay):-
	minmax(Board-ObjP,Player-Phase,BestPlay,3);
	nl,nl,write('Thinking a bit more for this one'),nl,nl,
	minmax(Board-ObjP,Player-Phase,BestPlay,5);
	nl,nl,write('Well, I am a bad looser. Lets just throw a move out of the box.'),nl,nl,
	choose_move(Board, Player-Phase, 1, BestPlay).


minmax(Board-ObjP,Player-Phase,BestSucc, Level) :-    
	value(Board-ObjP,Player-Phase,BestSucc,Value,Level),!, %level tem de ser impar para cair na msm cor q começa
	if(Value =< -40000, fail, true).


value(Board-ObjP,Player-Phase,BestSucc,Value,Depth) :-  
	valid_moves(Board, Player-Phase, MoveList),
	executeAll(Board-ObjP,Player-Phase,MoveList,BestSucc,Value,Depth). 



pc_move_avaliator(Board-ObjP, Color-Phase, Score, CC-CR/NC-NR):-

	(match_(Board, Color, NC-NR, RC-RR),  (RC >= 0; RR >= 0))->
		(
		ObjP == Color, Score is 1000;
		Score is -1050
		)
	;
	Score is 0.

executeAll(_-P,'W'-_,[],_, Score,_) :- P == 'W' -> Score is -100000; Score is 100000.
executeAll(_-P,'B'-_,[],_, Score,_) :- P == 'B' -> Score is -100000; Score is 100000.

% if depth of recursion reaches limit, value is infinite
executeAll(Board-P,'W'-_,_,_,Score, 0) :-  P == 'W' -> Score is -100000; Score is 100000.
executeAll(Board-P,'B'-_,_,_,Score, 0) :-  P == 'B' -> Score is -100000; Score is 100000.


executeAll(Board-ObjP,Player-Phase,[CC-CR/NC-NR|MoveList],BestSucc,Value,Depth) :-  

	move(Board, CC-CR/NC-NR, Player-Phase, NewBoard1),
	pc_move_avaliator(NewBoard1-ObjP, Player-Phase, Score, CC-CR/NC-NR),
	
	
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
		value(NewBoard-ObjP,NextPlayer-Phase,_,Value1,D),
		executeAll(Board-ObjP,Player-Phase,MoveList,BestSucc2,Value2,Depth),

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
			);
			(
				if(
					ThisValue < Value2,
					(Value=ThisValue, BestSucc=CC-CR/NC-NR),
					(Value=Value2, BestSucc=BestSucc2)
				)
			)
		)
	).


valid_moves(GameState, Color-capture, Moves):- 
	findall(Move, move(GameState, Move, Color-capture, NewState), Moves).

valid_moves(GameState, Color-drop, Moves):-
	findall(0-0/NC-NR, (get_piece(GameState, NR, NC, 'O'), move(GameState, 0-0/NC-NR, Color-Phase, NB)), Moves).

valid_moves(GameState, Color-peek, Moves):-
	next_color(Color, NColor),
	findall(0-0/NC-NR, get_piece(GameState, NR, NC, NColor), Moves);
	findall(Move, move(GameState, Move, Color-Phase, NewState), Moves).

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
