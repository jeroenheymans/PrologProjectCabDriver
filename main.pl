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
	write('No taxis left'),
	getAllCustomers(CustomersLeft),
	writeln(CustomersLeft).

% Loop over all the taxi's
% Pick a new customer, add him to the taxi and calculate the route
% to the customer. Then start loopInner/5
loop([Taxi|Taxis]):-
	retract(taxi(Taxi)),
	getMinETOP(Customer, ETOP),
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
	pickNextCustomer(Time, FStartID, Customer, Path, NewTime),
	%customer(Customer, 
	retract(customer(Customer, _, _, _, _)),
	append([Customer], [FirstID|Customers], NewCustomers),
	append(Path, [FStartID|TaxiPath], NewTaxiPath),
	loopInner(NewCustomers, NewTime, InTaxi, NewTaxiPath, EndPath).

% Take is also not yet filled but if we get here, this means we can't
% fill the taxi completely (no more customers left, no good customers to 
% pick up, ...). This makes sure we still get the path and that we know
% who the customers are in our taxi
loopInner(Customers, _, InTaxi, EndPath, EndPath):-
	InTaxi = Customers.
	
% Picks next customer, is not yet the best customer, just the first one
% that looks interesting
pickNextCustomer(Time, Node, Customer, Path, NewTime):-
	customer(Customer, ETOP, LTOP, CStartID, _),
	ETOP >= Time,
	minimumDistance(Node, CStartID, Path, PathTime),
	NewTime = Time + PathTime,
	NewTime =< LTOP.
	
getMinETOP(Customer, ETOP):-
	findall(Customer,
		(customer(CID, CETOP, _, _, _),
		 Customer = CETOP-CID),
		Customers),
	keysort(Customers, NewCustomers),
	NewCustomers = [ETOP-Customer|_].
	
