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
:- dynamic customerAvailable/4.

% Necessary includes
%:-['city_smaller.pl'].
:-reconsult('city.pl').
%:-reconsult('city_smallest.pl').
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].
    
main:-
	getAllTaxis(Taxis),
	setAllCustomersAvailable,
	getAllAvailableCustomers(Customers),
	listLength(Customers, TotalNrCustomers),
	write(TotalNrCustomers),writeln(' can be transported'),
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
	customerAvailable(Customer, waiting, _, [Path1-Time1]),!,
	customer(Customer, ETOP, LTOP, StartID, DestID),
	retract(customerAvailable(Customer, waiting, _, _)),
	startNode(Depot),
	minimumDistance(Depot, StartID, Path, _), % check on minimumtime
	loopInner([Customer], InTaxi, ETOP, Time1, FinalTime, Path, Path1, EndPath),
	(Customer =:= 1 -> writeln(EndDropoffPath) ; true),
	[CurrentNode|_] = EndPath,
	assert(taxiJob(Taxi, InTaxi, NewPath, FinalTime)),
	write('Taxi '),write(Taxi),write(' will transport: '),writeln(InTaxi),
	loop(Taxis).
	
%loopInner(Intaxi, IntaxiResult, FromTime, ToTime, FinalTime, FromPath, ToPath, FinalPath) 

loopInner([C1, C2, C3, C4], [C1, C2, C3, C4], FromTime, ToTime, FinalTime, [From|FromPath], [To|ToPath], FinalPath):-
	minimumDistance(From, To, FromToPath, FromToTime),
	FinalTime is FromTime + ToTime + FromToTime,
	append(FromPath, FromToPath, Temp),
	reverse(ToPath, ToPathReverse),
	append(Temp, ToPathReverse, FinalPath).
	
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
loopInner(Customers, InTaxi, FromTime, ToTime, FinalTime, [From|FromPath], [To|ToPath], FinalPath):-
	%pickNextCustomer(FromTime, From, Customer, [Top|AddFromPath], NewFromTime),
	pickNextCustomer(FromTime, From, PickUpCustomers),
	member(NewFromTime-[Customer-[Top|AddFromPath]], PickUpCustomers),
	\+member(Customer, Customers),
	customer(Customer, _, _, _, Destination),
	minimumDistance(Destination, To, AddToPath, AddToTime), %reverse
	NewToTime is ToTime + AddToTime,
	minimumDistance(Top, Destination, MiddlePath, MiddleTime),
	NewToTime + MiddleTime + NewFromTime =< 1440,
	retract(customerAvailable(Customer, _, _, _)),
	append(ToPath, AddToPath, NewToPath),
	append(FromPath, AddFromPath, NewFromPath), % hier gaat nog ne reverse moeten wss
	append([Customer], Customers, NewCustomers),
	loopInner(NewCustomers, InTaxi, NewFromTime, NewToTime, FinalTime, NewFromPath, NewToPath, FinalPath).

loopInner(Customers, InTaxi, FromTime, ToTime, FinalTime, [From|FromPath], [To|ToPath], FinalPath):-
	minimumDistance(From, To, FromToPath, FromToTime),
	FinalTime is FromTime + ToTime,
	append(FromPath, FromToPath, Temp),
	reverse(ToPath, ToPathReverse),
	append(Temp, ToPathReverse, FinalPath),
	InTaxi = Customers.
		 
transportLoop([]):-
	writeln('That is all folks!').
		 
transportLoop([Taxi|Jobs]):-
	taxiJob(Taxi, Customers, Path, FinalTime),
	write('Route for taxi '),write(Taxi),write(' (arrival: '),write(FinalTime),writeln('):'),
	routeLoop(Customers, Path),
	transportLoop(Jobs).
	
routeLoop(_, []).
	
routeLoop(Customers, [Node|Path]):-
	getCustomersPickupOnNode(Customers, Node, OnNode, NotOnNode),
	(OnNode = []
	 -> true
	 ; (write('On node '),write(Node),write(': '),writeln(OnNode))),
	routeLoop(NotOnNode, Path).
