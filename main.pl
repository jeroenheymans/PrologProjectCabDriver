% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main) or reconsult(main)
%   main.

% Necessary includes
%:-['city_smaller.pl'].
:-['city.pl'].
%:-['city_smallest.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].

test(CID1):-
	customer(CID1, ETOP1, LTOP1, Start1, Dest1),
	customer(CID2, ETOP2, LTOP2, Start2, Dest2),
	CID1 =\= CID2,
	minimumDistance(Start1, Start2, P, Time),
	NewLTOP2 is LTOP2 - Time,
	NewETOP2 is ETOP2 - Time,
	(NewETOP2 =< LTOP1 , NewLTOP2 >= ETOP1),
	write(CID1),write('&'),write(CID2),write(' ('),write(Time),write(') :'),write(ETOP1),write('-'),write(LTOP2),write('-'),write(NewETOP2),write('-'),writeln(NewLTOP2).
    
main:-
	getAllCustomers(Customers),
	keysort(Customers, SortedCustomers),
	getAllTaxis(Taxis),
	loop(SortedCustomers, Taxis).
	
loop([], _):-
	writeln('Everybody is delivered').

loop(_, []):-
	writeln('No taxis left').

loop([ETOP-Customer|Customers], [Taxi|Taxis]):-
	customer(Customer, ETOP, LTOP, StartID, DestID),
	startNode(DepotID),
	minimumDistance(DepotID, StartID, Path, Length),
	loopInner([Customer], Path, Customers, CustomersInTaxi),
	writeln(CustomersInTaxi),
	loop(Customers, Taxis).
	
loopInner([C1,C2,C3,C4], _, _, [C1,C2,C3,C4]).

loopInner(Customers, Path, OtherCustomers, CustomersInTaxi):-
	MinimumDistance is 999999,
	Customers = [First|_],
	customer(First, FirstETOP, FirstLTOP, FirstNode, _),
	customer(CID, ETOP, LTOP, Node, _),
	\+ memberchk(CID, Customers),
	memberchk(CID, OtherCustomers),
	minimumDistance(FirstNode, Node, Path, Length),
	NewETOP is ETOP - Length,
	NewLTOP is LTOP - Length,
	!,
	deleteFromList(CID, OtherCustomers, NewOtherCustomers),
	loopInner([CID|Customers], Path, NewOtherCustomers, CustomersInTaxi).
