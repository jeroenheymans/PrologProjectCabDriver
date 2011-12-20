% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main) or reconsult(main)
%   main.

% Necessary includes
:-['city_smaller.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].

% Test function, used for testing small parts of code before
% putting it in the rest of the code
testmain:-
    writeln('This is the testfunction').

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
    
loop(400, RemainingCustomers):-
    writeln('Times up!'),
    writeln(RemainingCustomers).
    %forall(transport(TaxiID,_,_),writeln(TaxiID)).
    
loop(Clock, CustomersToPickUp):-
    NewClock is Clock + 1,
    writeln(NewClock),
    loop(NewClock, CustomersToPickUp).
    
% Main loop
%   Clock = value of the internal clock
%   CustomersToPickUp = customers list still to be picked up
%       Format: DepartureTimeTaxi-CustomerID
%loop(Clock, CustomersToPickUp):-
%    nextCustomer(CustomersToPickUp,CustomerPickup-CustomerID,CustomersToPickUpRest),
%    NewClock is Clock + 1,
%    customer(CustomerID, ETOP, LTOP, NodeID, _),
%    startNode(StartID),
%    (NewClock =:= CustomerPickup 
%        -> (pickEmptyTaxi(Taxi),
%            assert(transport(Taxi, [CustomerID], NodeID, _, _)),
%            printNewCustomerInTaxi(CustomerID, Taxi, NewClock),
%            minimumDistance(StartID,NodeID,Path,_),
%            startTaxi(Taxi, Path),
%            CustomersToPickUpRest = [Top|_],
%            writeln(Top), 
%           loop(Clock, CustomersToPickUpRest))
%        ;  (moveAllTaxis(CustomersToPickUp),
%            loop(NewClock, CustomersToPickUp)            
%        )).
