% switch(+Variable, +[Value:Goal|Cases])
% Variable to be tested against Value, if it matches, Goal is called, otherwise the next case is tested.
switch(Variable, [Value:Goal|Cases]):-
	(Variable = Value -> % Test if the variable matches the value
	call(Goal); % If the variable matches the value, call the goal
	switch(Variable, Cases)). % Otherwise, test the next case

% succ(+Current, -Next)
% Next is the successor of Current in a sequence.
succ(Current, Next):- Next is Current+1. % Next is Current+1.

% abs(+Value, -AbsoluteValue)
% AbsoluteValue is the absolute value of Value.
abs(Value, AbsoluteValue):- Value >= 0, AbsoluteValue is Value. % Positive value case
abs(Value, AbsoluteValue):- Value < 0, AbsoluteValue is -Value. % Negative value case

% ======================= LIST UTILS =======================

% isList(+List)
% True if list is given.
isList([_|_]). % Non-empty list is a list.
isList([]). % Empty list is also a list.


%countElem_(+List, +Elem, +Acc, -Count)
% Count is the number of times Elem appears in List.
countElem_([], _, Acc, Acc):-!. % Base case
countElem_([H|T], E, Acc, Ret):- % Recursive case
	H==E-> 
		Acc1 is Acc+1, % If the head is equal to the element Increment the accumulator
		countElem_(T, E, Acc1, Ret); 
	countElem_(T, E, Acc, Ret).
	
% countElem(+List, +Elem, -Count)
% Count is the number of times Elem appears in List.	
countElem(List, Elem, Count):-countElem_(List, Elem, 0, Count).

% flatten(+List, -Flat)
% Turns List of lists into a Flat list.
flatten([], []). % Base case
flatten([H|T], Flat) :-
	 isList(H), !,
	 flatten(H, NewH),
	flatten(T, NewT),
	append(NewH, NewT, Flat).
flatten([H|T], [H|Flat]) :-
	flatten(T, Flat).

% insert_elem(+Index, +List, +Elem, -NewList)
% Inserts Elem at Index in List and returns the NewList.
insert_elem(0, [H|T], E, NewList):- append([E,H], T, NewList), !. % If the index is 0, just add the element
insert_elem(_, [], E, NewList):- NewList = [E],!. % If the list is empty, just add the element
insert_elem(I, [H|T], E, NewList):- % Otherwise, recursively add the element
	I > 0, 
	I1 is I-1,
	insert_elem(I1, T, E, L),
	append([H], L, NewList).
  
% delete_elem(+Index, +List, -NewList)
% Deletes the element at Index in List and returns the NewList.  
delete_elem(0, [_|T], NewList):- append([], T, NewList), !. % If the index is 0, just remove the element
delete_elem(I, [H|T], NewList):- % Otherwise, recursively remove the element
	I > 0, 
	I1 is I-1,
	delete_elem(I1, T, L),
	append([H], L, NewList).

% list_replace(+List, +Index, +Value, -New_List)
% Replaces the element at Index in List with Value and returns the New_List.
list_replace(List, Index, Value, New_List):-
	delete_elem(Index, List, List_Aux), % Delete the element at Index
	insert_elem(Index, List_Aux, Value, New_List). % Insert the new value at Index

% ====================== RUN LENGTH ENCODING ======================

% rle(+List, -Encoded)
% Encoded is the run length encoding of List.
% Run length encoding is a list of lists, where each sublist is of the form [Count, Value].
rle([], []):-!. % Base case
rle([X], [[1,X]]):-!. % Base case
rle([X|XT], [[Count, X]|RestEncoded]) :-
    rle(XT, [[SubCount, X]|RestEncoded]),
    succ(SubCount, Count),!.
rle([X|XT], [[1, X], [SubCount, Y] | RestEncoded]) :-
    rle(XT, [[SubCount, Y]|RestEncoded]),
    X \= Y,!.

% ====================== Matrix Generation ======================

% gen_2d_array(+N, +M, +C, -Matrix)
% Generates a N x M matrix with all elements equal to C.
gen_2d_array(0, _, C, []):-!.
gen_2d_array(N, M, C, [H|T]):- N > 0, gen_1d_array(M, C, H), N1 is N-1, gen_2d_array(N1, M, C, T).

% gen_1d_array(+N, +C, -Array)
% Generates a 1D array of size N with all elements equal to C.
gen_1d_array(0, C, []):-!.
gen_1d_array(N, C, [H|T]):- N > 0, H = C, N1 is N-1, gen_1d_array(N1, C, T).
