
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
	minmax(Board-ObjP,Player-Phase,BestSucc,Value,Level),!, %level tem de ser impar para cair na msm cor q começa
	write('Value:'),write(Value),nl,
	if(Value =< -40000, fail, true).


minmax(Board-ObjP,Player-Phase,BestSucc,Value,Depth) :-  
	valid_moves(Board, Player-Phase, MoveList),
	executeAll(Board-ObjP,Player-Phase,MoveList,BestSucc,Value,Depth). 



pc_move_avaliator2(Board-ObjP, Color-Phase, Score, CC-CR/NC-NR):-

	(match_(Board, Color, NC-NR, RC-RR),  (RC >= 0; RR >= 0))->
		(
		ObjP == Color, Score is 1000;
		Score is -1050
		)
	;
	Score is 0.

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

valid_moves(GameState, Color-capture, Moves):- 
	findall(Move, move(GameState, Move, Color-capture, NewState), Moves).

valid_moves(GameState, Color-drop, Moves):-
	findall(0-0/NC-NR, (get_piece(GameState, NR, NC, 'O'), move(GameState, 0-0/NC-NR, Color-Phase, NB)), Moves).

% mexi aqui ent posso ter feito asneira :)
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
