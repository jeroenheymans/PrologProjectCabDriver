% Functions that don't really fit anywhere specifically

startNode(Id):-node(Id,25,25).

square(Number,Result):-Result is Number*Number.

listLength([], 0).

listLength([_|Rest], Length):-
	listLength(Rest, NewLength),
	Length is NewLength + 1.
