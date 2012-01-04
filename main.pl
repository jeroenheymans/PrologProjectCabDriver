% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main) or reconsult(main)
%   main.

:- dynamic taxi/1.
:- dynamic customer/5.

% Necessary includes
%:-['city_smaller.pl'].
:-reconsult('city.pl').
%:-['city_smallest.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].
    
main:-
	getAllTaxis(Taxis),
	loop(Taxis).
	
loop([]):-
	write('No taxis left').

loop([Taxi|Taxis]):-
	retract(taxi(Taxi)),
	customer(Customer, ETOP, LTOP, StartID, DestID),
	retract(customer(Customer, ETOP, LTOP, StartID, DestID)),
	startNode(Depot),
	minimumDistance(Depot, StartID, Path, _), % check on minimumtime
	loopInner([Customer], ETOP, InTaxi, Path),
	writeln(InTaxi),
	writeln(Path),
	loop(Taxis).
	
loopInner([C1,C2,C3,C4], _, [C1,C2,C3,C4], _).

loopInner([FirstID|Customers], Time, InTaxi, [FStartID|TaxiPath]):-
	customer(CustomerID, CETOP, CLTOP, CStartID, CDestID),
	CustomerID =\= FirstID,
	minimumDistance(FStartID, CStartID, Path, PathTime),
	retract(customer(CustomerID, _, _, _, _)),
	append([CustomerID], [FirstID|Customers], NewCustomers),
	append(Path, [FStartID|TaxiPath], NewTaxiPath),
	loopInner(NewCustomers, Time, InTaxi, NewTaxiPath).

loopInner(Customers, _, InTaxi, _):-
	InTaxi = Customers.
