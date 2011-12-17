% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main) or reconsult(main)
%   main.

% Necessary includes
:-['city.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['functions.pl'].
:-['print.pl'].

% Main function, needs to be executed for this program
main(_):-
    writeln('Calculating departure times for taxis'),
    getDeparturesForPickupCustomers(CustomersToPickUp),
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
    writeln('Picking up customers'),
    loop(0, CustomersToPickUpSorted).
    
% Ending of the main loop
%   Clock = value of the internal clock
loop(Clock, []):-
    writeln('Finished calculations for all customers'),
    writeln(Clock).
    
loop(1440, RemainingCustomers):-
    writeln('Times up!'),
    writeln(RemainingCustomers),
    forall(transport(TaxiID,_,_),writeln(TaxiID)).
    
% Main loop
%   Clock = value of the internal clock
%   CustomersToPickUp = customers list still to be picked up
%       Format: DepartureTimeTaxi-CustomerID
loop(Clock, CustomersToPickUp):-
    nextCustomer(CustomersToPickUp,CustomerPickup-CustomerID,CustomersToPickUpRest),
    NewClock is Clock + 1,
    customer(CustomerID, ETOP, LTOP, NodeID, _),
    (NewClock =:= CustomerPickup 
        -> (pickEmptyTaxi(Taxi),
            assert(transport(Taxi, NodeID, [CustomerID])),
            printNewCustomerInTaxi(Taxi, CustomerID, NewClock),
           loop(Clock, CustomersToPickUpRest))
        ;  loop(NewClock, CustomersToPickUp)).
