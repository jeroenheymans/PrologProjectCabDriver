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
:- dynamic taxiJob/3.
:- dynamic customerAvailable/1.

% Necessary includes
%:-['city_smaller.pl'].
:-reconsult('city.pl').
%:-reconsult('city_smallest.pl').
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].

setAllCustomersAvailable:-
	findall(Customer,
		(customer(CID, _, _, _, _),
		 assert(customerAvailable(CID)),
		 Customer = CID),
		 _).
    
main:-
	getAllTaxis(Taxis),
	setAllCustomersAvailable,
	loop(Taxis).
	
loop([]):-
	write('No taxis left'),
	%getAllCustomers(CustomersLeft),
	%write('Customers left: '),writeln(CustomersLeft),
	getAllTaxiJobs(Jobs),
	transportLoop(Jobs).

% Loop over all the taxi's
% Pick a new customer, add him to the taxi and calculate the route
% to the customer. Then start loopInner/5
loop([Taxi|Taxis]):-
	retract(taxi(Taxi)),
	getMinETOP(Customer, ETOP),
	customerAvailable(Customer),
	customer(Customer, ETOP, LTOP, StartID, DestID),
	retract(customerAvailable(Customer)),
	startNode(Depot),
	minimumDistance(Depot, StartID, Path, _), % check on minimumtime
	loopInner([Customer], ETOP, InTaxi, Path, EndPath),
	assert(taxiJob(Taxi, InTaxi, EndPath)),
	write('Taxi '),write(Taxi),write(' will transport: '),writeln(InTaxi),
	loop(Taxis).
	
% Taxi is filled with 4 customers so copy the path we got as the endpath
loopInner([C1,C2,C3,C4], _, [C1,C2,C3,C4], EndPath, EndPath):-
	true. % fill this with calculation of the path to drop off these 4 customers

% Taxi is not yet filled with 4. Get the info where the taxi stand, take a
% new customer and calculate the route to him to pick him up
loopInner([FirstID|Customers], Time, InTaxi, [FStartID|TaxiPath], EndPath):-
	pickNextCustomer(Time, FStartID, Customer, Path, NewTime),
	retract(customerAvailable(Customer)),
	append([Customer], [FirstID|Customers], NewCustomers),
	append(Path, [FStartID|TaxiPath], NewTaxiPath),
	loopInner(NewCustomers, NewTime, InTaxi, NewTaxiPath, EndPath).

% Take is also not yet filled but if we get here, this means we can't
% fill the taxi completely (no more customers left, no good customers to 
% pick up, ...). This makes sure we still get the path and that we know
% who the customers are in our taxi
loopInner(Customers, _, InTaxi, EndPath, EndPath):-
	InTaxi = Customers.

getAllTaxiJobs(Jobs):-
	findall(Job,
		(taxiJob(Taxi, _, _),
		 Job = Taxi),
		 Jobs).	
		 
transportLoop([]):-
	writeln('That is all folks!').
		 
transportLoop([Taxi|Jobs]):-
	taxiJob(Taxi, Customers, Path),
	write('Route for taxi '),write(Taxi),writeln(':'),
	routeLoop(Customers, Path),
	transportLoop(Jobs).
	
routeLoop(_, []).
	
routeLoop(Customers, [Node|Path]):-
	getCustomersPickupOnNode(Customers, Node, OnNode, NotOnNode),
	(OnNode = []
	 -> true
	 ; (write('On node '),write(Node),write(': '),writeln(OnNode))),
	routeLoop(NotOnNode, Path).
