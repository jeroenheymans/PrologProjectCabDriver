% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main) or reconsult(main)
%   main.

:-['city.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['functions.pl'].

main(_):-
    getDeparturesForPickupCustomers(CustomersToPickUp),
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
    loop(CustomersToPickUpSorted).
    
loop(CustomersToPickUp):-
    nextCustomer(CustomersToPickUp,First,_),
    writeln(First).
