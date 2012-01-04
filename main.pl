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
:-consult('city.pl').
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
	getAllTaxis(Taxis),
	loop(Taxis).
	
loop([]):-
	write('No taxis left').

loop([Taxi|Taxis]):-
	retract(taxi(Taxi)),
	write('Picked taxi '),writeln(Taxi),
	retract(customer(Customer, ETOP, LTOP, StartID, DestID)),
	loopInner([Customer], InTaxi),
	write('In taxi: '),writeln(InTaxi),
	%loopInner([Customer], Path, Customers, CustomersInTaxi),
	%writeln(CustomersInTaxi).
	loop(Taxis).
	
loopInner([C1,C2,C3,C4], [C1,C2,C3,C4]).

loopInner(Customers, InTaxi):-
	(customer(Customer, ETOP, LTOP, StartID, DestID)
	-> (
	retract(customer(Customer, ETOP, LTOP, StartID, DestID)),
	append(Customers, [Customer], NewCustomers),
	loopInner(NewCustomers, InTaxi))
	; InTaxi = Customers).
