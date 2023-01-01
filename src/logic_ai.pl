% This file contains the definitions of AI Logic related predicates used in the program.

% choose_move(+GameState, +Player-Phase, +Level, -Move)/4
% GameState: The current state of the game.
% Player-Phase: The player and phase to make a move.
% Level: The level of the AI.
% Move: The move to be made.
% This predicate is used to choose the move to be made by the AI player.
choose_move(GameState, Player-Phase, Level, Move) :-
	( % If the level is 1, then the AI will make a random move.
		(Level == 1; Phase=peek; Phase=drop) -> 
			valid_moves(GameState, Player-Phase, Moves),
			random_member(Move, Moves)
	);
	( % If the level is 2, then the AI will calculate the best move.
		Level == 2 ->
			get_best_play(GameState-Player, Player-Phase, Move),
			write('Best play is : '),nl,
			write(Move)
	).

% get_best_play(+GameState-ObjP, +Player-Phase, -BestPlay)/3
% GameState-ObjP: The current state of the game and the objective player.
% Player-Phase: The player and phase to make a move.
% BestPlay: The best calculated play to be made.
% This predicate is used to get the next play to be made by the AI player.
get_best_play(Board-ObjP,Player-Phase,BestPlay):-
	minmax(Board-ObjP,Player-Phase,BestPlay,3); % Try to calculate the best play with depth 3.
	nl,nl,write('Thinking a bit more for this one'),nl,nl,
	minmax(Board-ObjP,Player-Phase,BestPlay,5); % Try to calculate the best play with depth 5.
	nl,nl,write('Cant figure a good move out... Lets just throw a move out of the box.'),nl,nl,
	choose_move(Board, Player-Phase, 1, BestPlay). % If the minmax algorithm fails, then make a random move.

% minmax(+GameState-ObjP, +Player-Phase, -BestSucc, +Level)/4
% GameState-ObjP: The current state of the game and the objective player.
% Player-Phase: The player and phase to make a move.
% BestSucc: The best calculated play to be made.
% Level: The level of the AI depth search (must be odd).
% This predicate is used to calculate the best play to be made by the AI player using minmax algorithm.
minmax(Board-ObjP,Player-Phase,BestSucc, Level) :-    
	value(Board-ObjP,Player-Phase,BestSucc,Value,Level),!, % Calculate the value of the best play.
	if(Value =< -40000, fail, true). % If the best value gotten is too low, then fail.

% value(+GameState-ObjP, +Player-Phase, -BestSucc, -Value, +Depth)/5
% GameState-ObjP: The current state of the game and the objective player.
% Player-Phase: The player and phase to make a move.
% BestSucc: The best calculated play to be made.
% Value: The value of the best play.
% Depth: The depth of the AI depth search.
% This predicate calculates the value and best move from all valid moves to be made by AI Player.
value(Board-ObjP,Player-Phase,BestSucc,Value,Depth) :-  
	valid_moves(Board, Player-Phase, MoveList), % Get all valid moves to be made by Player in Board in current Phase.
	executeAll(Board-ObjP,Player-Phase,MoveList,BestSucc,Value,Depth). % Calculate the value of all valid moves and returns the best one.

% executeAll(+GameState-ObjP, +Player-Phase, +MoveList, -BestSucc, -Value, +Depth)/6
% GameState-ObjP: The current state of the game and the objective player.
% Player-Phase: The player and phase to make a move.
% MoveList: The list of all valid moves to be made by Player in Board in current Phase.
% BestSucc: The best calculated play to be made.
% Value: The value of the best play.
% Depth: The depth of the AI depth search (decreeses by 1 in each recursive call).
% This predicate calculates the value and best move from all valid moves to be made by AI Player.

% if there are no more moves to be made, value is infinite
executeAll(_-P,'W'-_,[],_, Score,_) :- P == 'W' -> Score is -100000; Score is 100000.
executeAll(_-P,'B'-_,[],_, Score,_) :- P == 'B' -> Score is -100000; Score is 100000.

% if depth of recursion reaches limit, value is infinite
executeAll(Board-P,'W'-_,_,_,Score, 0) :-  P == 'W' -> Score is -100000; Score is 100000.
executeAll(Board-P,'B'-_,_,_,Score, 0) :-  P == 'B' -> Score is -100000; Score is 100000.

% Recursively calculate the value of all valid moves and returns the best one by value until the depth limit is reached.
% The value is calculated the best option to the current player and the worst outcome to the opponent.
executeAll(Board-ObjP,Player-Phase,[CC-CR/NC-NR|MoveList],BestSucc,Value,Depth) :-  

	move(Board, CC-CR/NC-NR, Player-Phase, NewBoard1), % Make the move in the board.
	pc_move_avaliator(NewBoard1-ObjP, Player-Phase, Score, CC-CR/NC-NR), % Calculate the score of the new move.
	
	% If the score is positive, then the move is good for the objective player.
	% If the score is negative, then the move is good for the opponent.
	if((Score > 0; Score < 0), 
		(choose_move(NewBoard1, Player-peek, 1, _-_/SC-SR), set_piece(NewBoard1, SR, SC, 'O', NewBoard)),
		(NewBoard = NewBoard1)
	),

	% Check if the game is over.
	game_over(NewBoard, Winner), 
	(
		% If the new board is a winning board, then the value is infinite.
		% It is infinite positive if the objective player is the winner and infinite negative if the opponent is the winner.
		Winner == ObjP,
		Value is 120000, BestSucc = CC-CR/NC-NR,!;

		Winner == Player,  
		Value is -120000, BestSucc = CC-CR/NC-NR,!;

		% If the game is not over, check more plays.
		next_color(Player, NextPlayer),
		D is Depth - 1, 
		value(NewBoard-ObjP,NextPlayer-Phase,_,Value1,D),
		executeAll(Board-ObjP,Player-Phase,MoveList,BestSucc2,Value2,Depth),

		% If the value of the current move is better than the best value, then the current move is the best move.
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


% pc_move_avaliator(+GameState-ObjP, +Player-Phase, -Score, +Move)/4
% GameState-ObjP: The current state of the game and the objective player.
% Player-Phase: The player and phase to make a move.
% Score: The score of the move.
% Move: The move to be made.
% This predicate checks if move leads to a 3match from the current player or the opponent.
pc_move_avaliator(Board-ObjP, Color-Phase, Score, CC-CR/NC-NR):-
	(
		match_(Board, Color, NC-NR, RC-RR),  (RC >= 0; RR >= 0)
	)->
		(
			ObjP == Color, Score is 1000; % Move leads to a 3match for the objective player.
			Score is -1050 % Move leads to a 3match for the opponent.
		);

	Score is 0. % Move does not lead to a 3match.

% valid_moves(+GameState, +Color-Phase, -Moves)/3
% GameState: The current state of the game.
% Color-Phase: The player and phase to make a move.
% Moves: The list of all valid moves to be made by Player in Board in current Phase.
% This predicate returns all valid moves to be made by Player in Board in current Phase.

% If the player is in the drop phase, then the valid moves are all the empty spaces not orthagonally adjacent to any friendly piece.
valid_moves(GameState, Color-drop, Moves):-
	findall(0-0/NC-NR, (get_piece(GameState, NR, NC, 'O'), move(GameState, 0-0/NC-NR, Color-Phase, NB)), Moves).

% If the player is in the capture phase, then the valid moves are all the orthagonal moves that can be made by any friendly piece.
valid_moves(GameState, Color-capture, Moves):- 
	findall(Move, move(GameState, Move, Color-capture, NewState), Moves).

% If the player is in the peek phase, then the valid moves are all the opponent pieces that can be taken.
valid_moves(GameState, Color-peek, Moves):-
	next_color(Color, NColor),
	findall(0-0/NC-NR, get_piece(GameState, NR, NC, NColor), Moves);
	findall(Move, move(GameState, Move, Color-Phase, NewState), Moves).
