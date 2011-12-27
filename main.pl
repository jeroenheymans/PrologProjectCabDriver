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
%:-['city.pl'].
:-['city_smallest.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].

% Main function, needs to be executed for this program
main(_):-
    getDeparturesForPickupCustomers(CustomersToPickUp),
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
    getAllTaxis(Taxis),
    loop(Taxis, CustomersToPickUpSorted).
    
loop([], Customers):-
	write('Remaining customers: '),writeln(Customers).
    
loop([Taxi|Taxis], Customers):-
	planTaxiRoute(Taxi, Customers, NewCustomers),
	loop(Taxis, NewCustomers).
	
planTaxiRoute(Taxi, [LeavingTime-Customer|Customers], NewCustomers):-
    write('Start planning taxi route for customer '),writeln(Customer),
	customer(Customer, ETOP, _, StartID, _),
    startNode(NodeID),
    minimumDistance(NodeID, StartID, Path, _),
    planTaxiRouteInner(Taxi, StartID, Path, ETOP, [Customer], [StartID], Customers, NewCustomers).
    
planTaxiRouteInner(Taxi, Destination, Path, Time, [], _, RCustomers, NewRCustomers):-
	writeln('Need to take another customer!').
    
planTaxiRouteInner(Taxi, Destination, Path, Time, [Customer|CIT], [CurrentNode|Other], RCustomers, NewRCustomers):- 
	write('Passing node '),writeln(CurrentNode),
	planTaxiRouteInner(Taxi, Destination, Path, Time, [Customer|CIT], Other, RCustomers, NewRCustomers).
	
% CIT = CustomersInTaxi
% RCustomers = RemainingCustomers
% Path = already followed path
% Time = time when reaching finish
% Customer = first customer in taxi (the one where we are going to
planTaxiRouteInner(Taxi, CurrentNode, Path, Time, [Customer|CIT], [CurrentNode], RCustomers, NewRCustomers):-
	customer(Customer, _, _, CurrentNode, Destination),
	minimumDistance(CurrentNode, Destination, CPath, Length),
	NewTime is Time + Length,
	append(Path, CPath, NewPath),
	write('Calculated new path '),writeln(CPath),
	planTaxiRouteInner(Taxi, Destination, NewPath, NewTime, [Customer|CIT], CPath, RCustomers, NewRCustomers).
