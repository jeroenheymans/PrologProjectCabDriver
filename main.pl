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
    
main:-
	getAllTaxis(Taxis),
	loop(Taxis).
	
loop([]):-
	write('No taxis left').

loop([Taxi|Taxis]):-
	retract(taxi(Taxi)),
	retract(customer(Customer, ETOP, LTOP, StartID, DestID)),
	loopInner([Customer], InTaxi),
	loop(Taxis).
	
loopInner([C1,C2,C3,C4], [C1,C2,C3,C4]).

loopInner(Customers, InTaxi):-
	(customer(Customer, ETOP, LTOP, StartID, DestID)
	-> (retract(customer(Customer, ETOP, LTOP, StartID, DestID)),
		append(Customers, [Customer], NewCustomers),
		loopInner(NewCustomers, InTaxi))
	; InTaxi = Customers).
