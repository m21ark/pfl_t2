% ======================= INPUT UTLS =======================

% read_number_acc(+Acc, -Ret)/2
% read a number digit by digit 
read_number_acc(Acc, Acc) :- peek_code(10), !.
read_number_acc(Acc, Ret) :- 
	\+ peek_code(10),
	get_code(C),
	char_code('0', Zero),
	D is C-Zero,
	D >= 0, D < 10,
	Nacc is Acc*10 + D,
	read_number_acc(Nacc, Ret).

% read_number(-X)/1
% read a number until \n
read_number(X) :- 
	read_number_acc(0, X),
	get_code(10).

% read_until_between(+Min, +Max, -Ret)/3
% read a Ret number until it is between Min and Max
read_until_between(Min, Max, Ret):-
	write('> '),read_number(V),
	between(Min,Max, V) -> Ret is V;
	write('Invalid number!'),nl,
	read_until_between(Min, Max, Ret).

% read_string(-String)/1
% read a string until \n
read_string("") :- peek_code(10),!,get_code(_).
read_string([C | T]) :- get_code(C), read_string(T).

% read_char(-Char)/1
% read a char and ignore the next one (\n)
read_char(C):-get_char(C), get_char(_).

% ======================= PRINT UTILS =======================

% print_n(+S, +N)/2
% print S N times
print_n(_ , 0):-!.
print_n(S, N):-
	N1 is N-1,
	write(S),
	print_n(S, N1).
	
% print_text(+Text, +Padding, +Space)/3
% print Text with Padding on both sides with Space spaces
print_text(Text, Padding, Space):-
	write(Padding),
	print_n(' ', Space),
	write(Text),
	print_n(' ', Space),
	write(Padding).

% print_Vpadd(+S, +L, +N)/3
% print S L times with N new lines to create vertical padding
print_Vpadd(_, _, 0):-!.
print_Vpadd(S, L, N):-
	write(S),
	print_n(' ', L) ,
	write(S),nl,
	N1 is N-1,
	print_Vpadd(S, L, N1).

% ======================= MENU DISPLAY =======================

% game_menu_show/0
% Prints the initial game menu
game_menu_show:-
	
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
    print_text('0) Go Back             ', '*' ,3), nl,
	print_Vpadd('*', L2, 1),	
	print_n('*', L3), nl.

% pc_menu_level_show/0
% Prints the AI level menu
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
	print_text(' 0) Quit               ', '*' ,3), nl,
	print_Vpadd('*', L2, 1),	
	print_n('*', L3), nl.

% ======================= GAME DISPLAY =======================

% display_level(+P1-P2)/2
% Displays the game menu and sets the game state
% P1 and P2 are the players. They can be human or pc
display_level_pc(P1-P2):-
	pc_menu_level_show,
	read_until_between(0,2, PC_Level), 
    (
        PC_Level = 0 -> !;
        set_game_state(P1-P2-PC_Level)
    ).

% set_game_state(+P1-P2-PC_Level)/3
% Sets the game state to the initial state
set_game_state(P1-P2-PC_Level) :- 
	initial_state(5-6, Board-WhiteTurn-WhiteCount-BlackCount),
	random_permutation([P1, P2], Turns),
	drop_phase(Board, WhiteCount, BlackCount, 1-PC_Level-Turns, NB),
	capture_phase(NB, 1-PC_Level-Turns, New_Board), 
	game_over(New_Board, Winner),
	display_game(New_Board),
	format('The winner is: ~w', [Winner]), ! .	

% ====================== BOARD PRINT ======================

% display_game(+Board)/1
% Displays the entire game board
display_game(Board):- 
	write('\n '), 
	display_game_(Board, -1), 
	print_n('-', 14),
	write('\n a  b  c  d  e\n').

% display_game_(+Board, +N)/2
% Displays the game board line by line
display_game_([], _):-!.
display_game_([H|T], N):- 
	
	\+ isList(H) -> 
	write(H), 
	write('  '),
	display_game_(T,N1);
	
	N1 is N+1,
	display_game_(H,N1),
	write('| '),write(N1),write('\n '), 
	display_game_(T,N1).
