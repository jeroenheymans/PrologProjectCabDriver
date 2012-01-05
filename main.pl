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

% Loop over all the taxi's
% Pick a new customer, add him to the taxi and calculate the route
% to the customer. Then start loopInner/5
loop([Taxi|Taxis]):-
	retract(taxi(Taxi)),
	customer(Customer, ETOP, LTOP, StartID, DestID),
	retract(customer(Customer, ETOP, LTOP, StartID, DestID)),
	startNode(Depot),
	minimumDistance(Depot, StartID, Path, _), % check on minimumtime
	loopInner([Customer], ETOP, InTaxi, Path, EndPath),
	writeln(InTaxi),
	%writeln(EndPath),
	loop(Taxis).
	
% Taxi is filled with 4 customers so copy the path we got as the endpath
loopInner([C1,C2,C3,C4], _, [C1,C2,C3,C4], EndPath, EndPath).

% Taxi is not yet filled with 4. Get the info where the taxi stand, take a
% new customer and calculate the route to him to pick him up
loopInner([FirstID|Customers], Time, InTaxi, [FStartID|TaxiPath], EndPath):-
	pickNextCustomer(Time, FStartID, CustomerID, Path),
	retract(customer(CustomerID, _, _, _, _)),
	append([CustomerID], [FirstID|Customers], NewCustomers),
	append(Path, [FStartID|TaxiPath], NewTaxiPath),
	loopInner(NewCustomers, Time, InTaxi, NewTaxiPath, EndPath).

% Take is also not yet filled but if we get here, this means we can't
% fill the taxi completely (no more customers left, no good customers to 
% pick up, ...). This makes sure we still get the path and that we know
% who the customers are in our taxi
loopInner(Customers, _, InTaxi, EndPath, EndPath):-
	InTaxi = Customers.
	
pickNextCustomer(Time, Node, Customer, Path):-
	customer(Customer, ETOP, LTOP, CStartID, _),
	ETOP >= Time,
	minimumDistance(Node, CStartID, Path, PathTime),
	Time + PathTime =< LTOP.
	
