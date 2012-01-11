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



%startNode(Start),findall(Customer,(customer(CID, ETOP, LTOP, Begin, Dest),minimumDistance(Begin, Dest, _, Time1),minimumDistance(Dest, Start, _, Time2), ETOP + Time1 + Time2 =< 1440,assert(customerAvailable(CID)),Customer = CID),Customers),listLength(Customers,T),writeln(T).

%startNode(Start),findall(Customer,(customer(CID, ETOP, LTOP, Begin, Dest),minimumDistance(Begin, Dest, _, Time1),minimumDistance(Dest, Start, _, Time2), LTOP + Time1 + Time2 =< 1440,assert(customerAvailable(CID)),Customer = CID),Customers),listLength(Customers,T),writeln(T).
