% This file contains the definitions of the main game predicate.

% Includes all the modules
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(system)).

% Includes all the files
:- ensure_loaded('utils.pl').
:- ensure_loaded('io.pl').
:- ensure_loaded('logic.pl').
:- ensure_loaded('logic_ai.pl').

% player(+Player)/1
% Player can be either human or computer
player(human).
player(computer).

% phase(+Phase)/1
% Phase can be either capture, drop or peek
phase(drop).
phase(capture).
phase(peek).

% play/0
% Main function that starts the game
play:-
	repeat,
	clear,
	game_menu_show, % Shows the game menu
	read_until_between(0,4, OPT), % Reads the input option
	switch(OPT, [
		0: (!), % Exits the game
		1: set_game_state(human-human-0), % Sets the game state to human vs human
		2: display_level_pc(human-computer), % Sets the game state to human vs computer
		3: display_level_pc(computer-computer), % Sets the game state to computer vs computer
		4: display_rules % Displays the rules
	]),
	OPT = 0 -> !; % If 0 is selected, exit the game
	nl, write('End of game.'), nl, nl, fail. % Restart the game loop