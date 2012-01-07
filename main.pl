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
:- dynamic taxiJob/4.
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
		 
orderClosestCustomersInner([], _, []).

orderClosestCustomersInner([Customer|Customers], Node, [Distance-Customer|NewCustomers]):-
	orderClosestCustomersInner(Customers, Node, NewCustomers),
	customer(Customer, _, _, _, CNode),
	minimumDistance(Node, CNode, _, Distance).	
		 
orderClosestCustomers(Customers, Node, NewCustomers):-
	orderClosestCustomersInner(Customers, Node, UnorderedCustomers),
	keysort(UnorderedCustomers, OrderedCustomers),
	removeKeys(OrderedCustomers, NoKeysOrderedCustomers),
	reverse(NoKeysOrderedCustomers, NewCustomers).

routeBetweenCustomers([], Node, Path, NewPath, Time, Time):-
	NewPath = [Node|Path].
	
routeBetweenCustomers([Customer|Customers], Node, Path, EndPath, Time, NewTime):-
	customer(Customer, _, _, _, EndNode),
	minimumDistance(Node, EndNode, [NewNode|P], PathTime),
	append(P, Path, NewPath),
	TempTime is Time + PathTime,
	routeBetweenCustomers(Customers, NewNode, NewPath, EndPath, TempTime, NewTime).
		 
calculateDropOffPath(Customers, Node, Time, Path, NewTime):-
	orderClosestCustomers(Customers, Node, NewCustomers),
	routeBetweenCustomers(NewCustomers, Node, [], Path, Time, NewTime).
    
main:-
	getAllTaxis(Taxis),
	setAllCustomersAvailable,
	loop(Taxis).
	
loop([]):-
	writeln('No taxis left'),
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
	loopInner([Customer], ETOP, InTaxi, Path, EndPath, EndTime),
	[CurrentNode|_] = EndPath,
	calculateDropOffPath(InTaxi, CurrentNode, EndTime, [Top|DropOffPath], NewTime), 
	append([Top|DropOffPath], EndPath, [_|Temp]),
	minimumDistance(Top, Depot, P, Time),
	append(P, Temp, Temp2),
	reverse(Temp2, NewPath),
	FinalTime is NewTime + Time,
	assert(taxiJob(Taxi, InTaxi, NewPath, FinalTime)),
	write('Taxi '),write(Taxi),write(' will transport: '),writeln(InTaxi),
	loop(Taxis).
	
% Taxi is filled with 4 customers so copy the path we got as the endpath
loopInner([C1,C2,C3,C4], EndTime, [C1,C2,C3,C4], EndPath, EndPath, EndTime):-
	true. % fill this with calculation of the path to drop off these 4 customers

% Taxi is not yet filled with 4. Get the info where the taxi stand, take a
% new customer and calculate the route to him to pick him up
loopInner([FirstID|Customers], Time, InTaxi, [FStartID|TaxiPath], EndPath, EndTime):-
	pickNextCustomer(Time, FStartID, Customer, Path, NewTime),
	retract(customerAvailable(Customer)),
	append([Customer], [FirstID|Customers], NewCustomers),
	append(Path, [FStartID|TaxiPath], NewTaxiPath),
	loopInner(NewCustomers, NewTime, InTaxi, NewTaxiPath, EndPath, EndTime).

% Take is also not yet filled but if we get here, this means we can't
% fill the taxi completely (no more customers left, no good customers to 
% pick up, ...). This makes sure we still get the path and that we know
% who the customers are in our taxi
loopInner(Customers, EndTime, InTaxi, EndPath, EndPath, EndTime):-
	InTaxi = Customers.

getAllTaxiJobs(Jobs):-
	findall(Job,
		(taxiJob(Taxi, _, _, _),
		 Job = Taxi),
		 Jobs).	
		 
transportLoop([]):-
	writeln('That is all folks!').
		 
transportLoop([Taxi|Jobs]):-
	taxiJob(Taxi, Customers, Path, FinalTime),
	write('Route for taxi '),write(Taxi),write(' (arrival: '),write(FinalTime),writeln('):'),
	(Taxi =:= 0 -> writeln(Path) ; true),
	routeLoop(Customers, Path),
	transportLoop(Jobs).
	
routeLoop(_, []).
	
routeLoop(Customers, [Node|Path]):-
	getCustomersPickupOnNode(Customers, Node, OnNode, NotOnNode),
	(OnNode = []
	 -> true
	 ; (write('On node '),write(Node),write(': '),writeln(OnNode))),
	routeLoop(NotOnNode, Path).
