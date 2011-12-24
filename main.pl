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
%:-['city.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].

:-dynamic clock/1.

% Test function, used for testing small parts of code before
% putting it in the rest of the code
testmain:-
    writeln('This is the testfunction').

% Main function, needs to be executed for this program
main(_):-
    getDeparturesForPickupCustomers(CustomersToPickUp),
    %CustomersToPickUp = [1038-0,233-1,587-2],
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
    assert(clock(0)),
    loop(0, CustomersToPickUpSorted).
    
loop(3000, RemainingCustomers):-
    printTimesUp(RemainingCustomers).
          
loop(Clock, []):-
    retract(clock(Clock)),
    NewClock is Clock + 1,
    assert(clock(NewClock)),
    getTaxisInTransport(TaxisInTransport),
    moveTaxis(TaxisInTransport),
    loop(NewClock, []).
    
loop(Clock, RemainingCustomers):-
    retract(clock(Clock)),
    NewClock is Clock + 1,
    assert(clock(NewClock)),
    customersToPickupNow(NewClock, RemainingCustomers, CustomersNowToPickUp, CustomersToPickUpLater),
    printCustomersToPickUpNow(CustomersNowToPickUp),
    sendTaxisToCustomers(CustomersNowToPickUp),
    getTaxisInTransport(TaxisInTransport),
    moveTaxis(TaxisInTransport),
    loop(NewClock, CustomersToPickUpLater).
