:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(between)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(system)).

:- ensure_loaded('utils.pl').
:- ensure_loaded('io.pl').
:- ensure_loaded('logic.pl').
:- ensure_loaded('logic_utils.pl').
:- ensure_loaded('logic_ai.pl').
% MAIN
player(human).
player(computer).

phase(drop).
phase(capture).
phase(peek).

play:-
	repeat,
	game_menu_show,
	read_until_between(0,3, OPT),
	switch(OPT, [
		0: (!),
		1: set_game_state(human-human-0),
		2: display_level_pc(human-computer),
		3: display_level_pc(computer-computer)
	]),
	OPT = 0 -> !;
	nl, write('End of game.'), nl, nl, fail.