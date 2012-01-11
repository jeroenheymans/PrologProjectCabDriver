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
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].

:-dynamic clock/1.
:-dynamic customerAvailable/3.

setAllCustomersAvailable:-
	startNode(Start),
	findall(Customer,
		(customer(CID, ETOP, LTOP, Begin, Dest),
		 minimumDistance(Begin, Dest, _, Time1),
		 minimumDistance(Dest, Start, _, Time2),
		 ETOP + Time1 + Time2 =< 1440,
		 assert(customerAvailable(CID, Time1, Time2)),
		 Customer = CID),
		 _).
		 
% Main function, needs to be executed for this program
main:-
	setAllCustomersAvailable,
    getDeparturesForPickupCustomers(CustomersToPickUp),
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
    assert(clock(0)),
    getAllTaxis(Taxis),
    initTaxis(Taxis),
    loop(0, CustomersToPickUpSorted).
    
loop(5000, RemainingCustomers):-
	listLength(RemainingCustomers, L),
	write('# customers remaining: '),writeln(L),
    printTimesUp(RemainingCustomers).
          
loop(Clock, []):-
    incrementClock(Clock, NewClock),
    getTaxisInTransport(TaxisInTransport),
    moveTaxis(TaxisInTransport),
    loop(NewClock, []).
    
loop(Clock, RemainingCustomers):-
    incrementClock(Clock, NewClock),
    customersToPickupNow(NewClock, RemainingCustomers, CustomersNowToPickUp, CustomersToPickUpLater),
    %printCustomersToPickUpNow(CustomersNowToPickUp),
    getAvailableTaxis(AvailableTaxis),
    sendTaxisToCustomers(CustomersNowToPickUp, AvailableTaxis, NoTaxisSend),
    (\+NoTaxisSend = []
    -> (append(CustomersToPickUpLater, NoTaxisSend, CustomersToPickUp))
    	%writeln(NoTaxisSend),
    	%writeln(CustomersToPickUp))
    ; CustomersToPickUp = CustomersToPickUpLater),
    getTaxisInTransport(TaxisInTransport),
    moveTaxis(TaxisInTransport),
    loop(NewClock, CustomersToPickUp).
    
incrementClock(Clock, NewClock):-
    retract(clock(Clock)),
    NewClock is Clock + 1,
    assert(clock(NewClock)).

