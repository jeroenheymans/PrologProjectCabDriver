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
	
planTaxiRoute(Taxi, Customers, NewCustomers):-
	NewCustomers = Customers.
