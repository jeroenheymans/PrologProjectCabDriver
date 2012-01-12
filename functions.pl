% Functions that don't really fit anywhere specifically

startNode(Id):-node(Id,24,24).

square(Number,Result):-Result is Number*Number.

listLength([], 0).

listLength([_|Rest], Length):-
	listLength(Rest, NewLength),
	Length is NewLength + 1.
	
removeKeys([], []).
removeKeys([_-First|List], NewList):-
	removeKeys(List, RestList),
	NewList = [First|RestList].
	
deleteFromList(X, [], X).
deleteFromList(X, [_-X|Rest], Rest):- !.
deleteFromList(X, [Y-Z|Rest], [Y-Z|Deleted]):- deleteFromList(X, Rest, Deleted).

minimum(M,N,M):-M=<N.
minimum(M,N,N):-N=<M.

maximum(M,N,M):-N=<M.
maximum(M,N,N):-M=<N.
