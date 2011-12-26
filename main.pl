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

test(t(X,Y),Z):-
	Z is X+Y.

% Test function, used for testing small parts of code before
% putting it in the rest of the code
    %CustomersToPickUp = [1038-0,233-1,587-2],
testmain:-
	test(t(1,2),X),
	writeln(X).
    %getDeparturesForPickupCustomers(CustomersToPickUp),
    %keysort(CustomersToPickUp, CustomersToPickUpSorted),
    %writeln(CustomersToPickUpSorted).
    
newmain:-
    getDeparturesForPickupCustomers(CustomersToPickUp),
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
	getAllTaxis(Taxis),
	newloop(CustomersToPickUpSorted, Taxis).
	
newloop([],Taxis):-
	write('All customers will get a cab, remaining taxis: '),writeln(Taxis).

newloop(Customers,[]):-
	write('Taxis are full!!! Customers left: '),writeln(Customers).
	
newloop([LeavingTime-CID|Customers], [Taxi|Taxis]):-
	customer(CID, ETOP, LTOP, SID, FID),
	startNode(PID),
	write('Customer '),writeln(CID),
	minimumDistance(PID, SID, Path, Length),
	planTaxiRoute(Taxi, [CID], Path, ETOP, Path, Customers),
	newloop(Customers, Taxis).

planTaxiRoute(Taxi, Customers, Path, ETOP, [], RemainingCustomers):-
	writeln('Send him home').

planTaxiRoute(Taxi, Customers, Path, ETOP, [Node], RemainingCustomers):-
	writeln('Someone left on this spot?').
	
% TaxiID
% Customers ID
% Complete path
% time taxi will end this path
% Last path (can be used to walk through
% remaining customers
planTaxiRoute(Taxi, Customers, Path, ETOP, [Node|RestPath], RemainingCustomers):-
	planTaxiRoute(Taxi, Customers, Path, ETOP, RestPath, RemainingCustomers).

% Main function, needs to be executed for this program
main(_):-
    getDeparturesForPickupCustomers(CustomersToPickUp),
    keysort(CustomersToPickUp, CustomersToPickUpSorted),
    assert(clock(0)),
    getAllTaxis(Taxis),
    initTaxis(Taxis),
    loop(0, CustomersToPickUpSorted).
    
loop(1440, RemainingCustomers):-
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

