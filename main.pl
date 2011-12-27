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
    
deleteLeavingTimes([], []).
    
deleteLeavingTimes([_-Customer|Customers], [Customer|NewCustomers]):-
	deleteLeavingTimes(Customers, NewCustomers).

% Main function, needs to be executed for this program
main(_):-
    getDeparturesForPickupCustomers(CustomersToPickUp),
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
    deleteLeavingTimes(CustomersToPickUpSorted,Customers),
    writeln(Customers),
    getAllTaxis(Taxis),
    %CustomersToPickUpSorted = [19-6, 30-9],
    loop(Taxis, Customers).
    
loop([], Customers):-
	write('Remaining customers: '),writeln(Customers).
    
loop([Taxi|Taxis], Customers):-
	planTaxiRoute(Taxi, Customers, NewCustomers),
	loop(Taxis, NewCustomers).
	
planTaxiRoute(Taxi, [Customer|Customers], NewCustomers):-
    write('Start planning taxi route for customer '),writeln(Customer),
	customer(Customer, ETOP, _, StartID, Destination),
    startNode(NodeID),
    minimumDistance(NodeID, StartID, PathToCustomer, _),
    minimumDistance(StartID, Destination, PathToDestination, Length),
    TotalTime is ETOP + Length,
    append(PathToCustomer, PathToDestination, TotalPath),
    planTaxiRouteInner(Taxi, Destination, TotalPath, TotalTime, [Customer], PathToDestination, Customers, NewCustomers).
    
areIdentical(X, Y):-X==Y.
    
planTaxiRouteInner(Taxi, Destination, Path, Time, [], _, [], []):-
	writeln('Need to take another customer!').
    
planTaxiRouteInner(Taxi, NodeID, Path, Time, [], _, Customers, NewRCustomers):-
	writeln('Need to take another customer!'),
	getBestCustomer(Customers, NodeID, Time, Customer),
	exclude(areIdentical(Customer), Customers, NewCustomers),
	(Customer = []
	-> NewRCustomers = NewCustomers
	; (
	write('Taking new customer: '),writeln(Customer),
	customer(Customer, ETOP, _, StartID, Destination),
    minimumDistance(NodeID, StartID, PathToCustomer, TimeToCustomer),
    minimumDistance(StartID, Destination, PathToDestination, Length),
    TotalTime is Time + TimeToCustomer + Length,
    append(Path, PathToCustomer, MidPath),
    append(MidPath, PathToDestination, TotalPath),
    planTaxiRouteInner(Taxi, Destination, TotalPath, TotalTime, [Customer], PathToDestination, NewCustomers, NewRCustomers))).
    
planTaxiRouteInner(Taxi, Destination, Path, Time, CIT, [CurrentNode|Other], RCustomers, NewRCustomers):- 
	planTaxiRouteInner(Taxi, Destination, Path, Time, CIT, Other, RCustomers, NewRCustomers).
	
% CIT = CustomersInTaxi
% RCustomers = RemainingCustomers
% Path = already followed path
% Time = time when reaching finish
% Customer = first customer in taxi (the one where we are going to
planTaxiRouteInner(Taxi, FinishNode, Path, Time, CIT, [FinishNode], RCustomers, NewRCustomers):-
	writeln(CIT),
	writeln(FinishNode),
	dropOffCustomers(CIT, FinishNode, DroppedOff, NewCIT),
	(DroppedOff = []
	-> true
	; (write('Dropped off: '),writeln(DroppedOff))),
	planTaxiRouteInner(Taxi, FinishNode, Path, Time, NewCIT, [], RCustomers, NewRCustomers).
	%customer(Customer, _, _, CurrentNode, Destination),
	%minimumDistance(CurrentNode, Destination, CPath, Length),
	%NewTime is Time + Length,
	%append(Path, CPath, NewPath),
	%write('Calculated new path '),writeln(CPath),
	%planTaxiRouteInner(Taxi, Destination, NewPath, NewTime, [Customer|CIT], CPath, RCustomers, NewRCustomers).
