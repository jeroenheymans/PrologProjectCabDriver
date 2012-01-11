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

% Set all customers that we know as available
setAllCustomersAvailable:-
	startNode(Start),
	findall(Customer,
		(customer(CID, _, LTOP, Begin, Dest),
		 minimumDistance(Begin, Dest, _, Time1),
		 minimumDistance(Dest, Start, _, Time2),
		 ETOP + Time1 + Time2 =< 1440,
		 assert(customerAvailable(CID)),
		 Customer = CID),
		 _).
		 
% Inner function for orderClosestCustomers/3
% stopcondition
orderClosestCustomersInner([], _, []).

% Inner function for orderClosestCustomers/3
%	+Customer = current customer
%	+Customers = other customers still to calculate
%	+Node = node where we are now
%	-Distance = distance to dropoff from Customer starting from Node
%	-NewCustomers = calculated customers
orderClosestCustomersInner([Customer|Customers], Node, [Distance-Customer|NewCustomers]):-
	orderClosestCustomersInner(Customers, Node, NewCustomers),
	customer(Customer, _, _, _, CNode),
	minimumDistance(Node, CNode, _, Distance).	
		 
% Order to the closest customers
%	+Customers = customers to order
%	+Node = node where we are now
%	-NewCustomers = ordered customers
orderClosestCustomers(Customers, Node, NewCustomers):-
	orderClosestCustomersInner(Customers, Node, UnorderedCustomers),
	keysort(UnorderedCustomers, OrderedCustomers),
	removeKeys(OrderedCustomers, NoKeysOrderedCustomers),
	reverse(NoKeysOrderedCustomers, NewCustomers).

% Calculate route between customers, endcondition
%	+Customers = []
%	+Node = node where we are now
%	+Path = path we already followed
%	-NewPath = final path
%	+Time = time it already is
%	-NewTime = Time = final time
routeBetweenCustomers([], Node, Path, NewPath, Time, Time):-
	NewPath = [Node|Path].
	
% Calculate route between customers
% 	+Customer = customer to calculate his path
%	+Customers = other customers still to calculate
%	+Node = node where we are now
%	+Path = path we already followed
%	-EndPath = final path we have to follow
%	+Time = time it already is
%	-NewTime = final time necessary to follow EndPath
routeBetweenCustomers([Customer|Customers], Node, Path, EndPath, Time, NewTime):-
	customer(Customer, _, _, _, EndNode),
	minimumDistance(Node, EndNode, [NewNode|P], PathTime),
	append(P, Path, NewPath),
	TempTime is Time + PathTime,
	routeBetweenCustomers(Customers, NewNode, NewPath, EndPath, TempTime, NewTime).
		 
% Calculate the drop off path as soon as we picked up customers
% 	+Customer = customers to drop off
%	+Node = node where we are now
%	+Time = time it is now
%	-Path = path we take to drop off customers
%	-NewTime = time it takes to follow the Path
calculateDropOffPath(Customers, Node, Time, Path, NewTime):-
	orderClosestCustomers(Customers, Node, NewCustomers),
	routeBetweenCustomers(NewCustomers, Node, [], Path, Time, NewTime).
    
main:-
	getAllTaxis(Taxis),
	setAllCustomersAvailable,
	loop(Taxis).
	
loop([]):-
	writeln('No taxis left'),
	getAllAvailableCustomers(CustomersLeft),
	listLength(CustomersLeft, Total),
	write('Customers left ('),write(Total),write('): '),writeln(CustomersLeft),
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
% 	+C1,C2,C3,C4 = the customers in the taxi
%	+EndTime = Time that all customers are picked up
%	+EndPath = Path to pick up all the customers, starting at depot
loopInner([C1,C2,C3,C4], EndTime, [C1,C2,C3,C4], EndPath, EndPath, EndTime):-
	true.

% Taxi is not yet filled with 4. Get the info where the taxi stand, take a
% new customer and calculate the route to him to pick him up
% 	+FirstID = ID of the previous picked up customer
%	+Customers = other customers picked up
%	+Time = time it took to get to previous picked up customer
%	-InTaxi = final result of all picked up customers
%	+FStartID = last node where taxi was
%	+TaxiPath = path the taxi has already done
%	-EndPath = final path the taxi needs to do
%	-EndTime = final time it took to pick up all the customers
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
%	-InTaxi = final result of all picked up customers
%	-EndPath = final path the taxi needs to do
%	-EndTime = final time it took to pick up all the customers
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
