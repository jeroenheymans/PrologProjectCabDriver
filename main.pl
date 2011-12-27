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
    %getDeparturesForPickupCustomers(CustomersToPickUp),
    %keysort(CustomersToPickUp, CustomersToPickUpSorted),
    %getAllTaxis(Taxis),
    Taxis = [0],
    CustomersToPickUpSorted = [19-6],
    loop(Taxis, CustomersToPickUpSorted).
    
loop([], Customers):-
	write('Remaining customers: '),writeln(Customers).
    
loop([Taxi|Taxis], Customers):-
	planTaxiRoute(Taxi, Customers, NewCustomers),
	loop(Taxis, NewCustomers).
	
planTaxiRoute(Taxi, [LeavingTime-Customer|Customers], Customers):-
    write('Start planning taxi route for customer '),writeln(Customer),
	customer(Customer, ETOP, _, StartID, Destination),
    startNode(NodeID),
    minimumDistance(NodeID, StartID, PathToCustomer, _),
    minimumDistance(StartID, Destination, PathToDestination, Length),
    TotalTime is ETOP + Length,
    append(PathToCustomer, PathToDestination, TotalPath),
    planTaxiRouteInner(Taxi, Destination, TotalPath, TotalTime, [Customer], PathToDestination, Customers, NewCustomers).
    
planTaxiRouteInner(Taxi, Destination, Path, Time, [], _, RCustomers, NewRCustomers):-
	writeln('Need to take another customer!').
    
planTaxiRouteInner(Taxi, Destination, Path, Time, CIT, [CurrentNode|Other], RCustomers, NewRCustomers):- 
	write('Passing node '),writeln(CurrentNode),
	planTaxiRouteInner(Taxi, Destination, Path, Time, CIT, Other, RCustomers, NewRCustomers).
	
% CIT = CustomersInTaxi
% RCustomers = RemainingCustomers
% Path = already followed path
% Time = time when reaching finish
% Customer = first customer in taxi (the one where we are going to
planTaxiRouteInner(Taxi, FinishNode, Path, Time, CIT, [FinishNode], RCustomers, NewRCustomers):-
	%writeln(Customer),
	writeln(CIT),
	writeln(FinishNode).
	dropOffCustomers(CIT, NewCIT, FinishNode).
	%customer(Customer, _, _, CurrentNode, Destination),
	%minimumDistance(CurrentNode, Destination, CPath, Length),
	%NewTime is Time + Length,
	%append(Path, CPath, NewPath),
	%write('Calculated new path '),writeln(CPath),
	%planTaxiRouteInner(Taxi, Destination, NewPath, NewTime, [Customer|CIT], CPath, RCustomers, NewRCustomers).
