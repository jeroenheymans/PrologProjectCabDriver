% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main) or reconsult(main)
%   main.

:- dynamic taxi/1.
:- dynamic customer/5.
:- dynamic taxiJob/4.
:- dynamic customerAvailable/3.

% Necessary includes
%:-['city_smaller.pl'].
:-reconsult('city.pl').
%:-reconsult('city_1taxi.pl').
%:-reconsult('city_smallest.pl').
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].
    
% Main, responsible for the entire program
% This takes all the taxis in a list and sets the
% necessary availableCustomers
main:-
	printStart,
	getAllTaxis(Taxis),
	setAllCustomersAvailable,
	getAllAvailableCustomers(Customers),
	printTransportableCustomers(Customers),
	loop(Taxis).
	
% Stopcondition for the big loop, this means
% there are no taxis left
% 	+Taxis = []
loop([]):-
	printNoTaxisLeft,
	getAllAvailableCustomers(CustomersLeft),
	getAllTaxiJobs(Jobs),
	printTransportLoop(Jobs),
	printCustomersLeft(CustomersLeft).

% Loop over all the taxi's
% Pick a new customer, add him to the taxi and calculate the route
% to the customer. Then start loopInner/8 that will add other
% customers to the taxi
%	+Taxi = taxi to use now
%	+Taxis = remaining taxis
loop([Taxi|Taxis]):-
	retract(taxi(Taxi)),
	getMinETOP(Customer, ETOP),
	customerAvailable(Customer, _, [Path1-Time1]),!,
	customer(Customer, ETOP, _, StartID, _),
	retract(customerAvailable(Customer, _, _)),
	startNode(Depot),
	minimumDistance(Depot, StartID, Path, _), % check on minimumtime
	loopInner([Customer], InTaxi, ETOP, Time1, FinalTime, Path, Path1, EndPath),
	assert(taxiJob(Taxi, InTaxi, EndPath, FinalTime)),
	printCustomersInTaxi(Taxi, InTaxi),
	loop(Taxis).

% Stopcondition for the big loop
% This is called when a taxi has 4 customers
% 	+C1,C2,C3,C4 = the 4 customers in a taxi
%	+FromTime = time necessary for the from-part of the taxi path
%	+ToTime = time necessary for the to-part of the taxi path
%	-FinalTime = total time necessary for the taxi to travel (also known as the finish time)
%	+From = first element of the already taken from-path
%	+FromPath = the already taken from-path
%	+To = first element of the already taken to-path
%	+ToPath = the already taken to-path
%	-FinalPath = total path that the taxi has to follow
loopInner([C1, C2, C3, C4], [C1, C2, C3, C4], FromTime, ToTime, FinalTime, [From|FromPath], [To|ToPath], FinalPath):-
	minimumDistance(From, To, FromToPath, FromToTime),
	FinalTime is FromTime + ToTime + FromToTime,
	reverse(FromPath, FromPathReverse),
	reverse(FromToPath, FromToPathReverse),
	append(FromPathReverse, FromToPathReverse, Temp),
	append(Temp, ToPath, FinalPath).
	
% Taxi is not yet filled with 4 customers so we try to add a new customer here
% 	+C1,C2,C3,C4 = the 4 customers in a taxi
%	+FromTime = time necessary for the from-part of the taxi path
%	+ToTime = time necessary for the to-part of the taxi path
%	-FinalTime = total time necessary for the taxi to travel (also known as the finish time)
%	+From = first element of the already taken from-path
%	+FromPath = the already taken from-path
%	+To = first element of the already taken to-path
%	+ToPath = the already taken to-path
%	-FinalPath = total path that the taxi has to follow
loopInner(Customers, InTaxi, FromTime, ToTime, FinalTime, [From|FromPath], [To|ToPath], FinalPath):-
	pickNextCustomer(FromTime, From, PickUpCustomers),
	member(NewFromTime-[Customer-[Top|AddFromPath]], PickUpCustomers),
	\+member(Customer, Customers),
	customer(Customer, _, _, _, Destination),
	minimumDistance(Destination, To, AddToPath, AddToTime),
	NewToTime is ToTime + AddToTime,
	minimumDistance(Top, Destination, _, MiddleTime),
	NewToTime + MiddleTime + NewFromTime =< 1440,
	retract(customerAvailable(Customer, _, _)),
	reverse(AddToPath, Temp),
	append(Temp, ToPath, NewToPath),
	append([Top|AddFromPath], FromPath, NewFromPath),
	append([Customer], Customers, NewCustomers),
	loopInner(NewCustomers, InTaxi, NewFromTime, NewToTime, FinalTime, NewFromPath, NewToPath, FinalPath).

% This is called when a taxi has not yet 4 customers AND there could be no more customer added
% 	+C1,C2,C3,C4 = the 4 customers in a taxi
%	+FromTime = time necessary for the from-part of the taxi path
%	+ToTime = time necessary for the to-part of the taxi path
%	-FinalTime = total time necessary for the taxi to travel (also known as the finish time)
%	+From = first element of the already taken from-path
%	+FromPath = the already taken from-path
%	+To = first element of the already taken to-path
%	+ToPath = the already taken to-path
%	-FinalPath = total path that the taxi has to follow
loopInner(Customers, InTaxi, FromTime, ToTime, FinalTime, [From|FromPath], [To|ToPath], FinalPath):-
	minimumDistance(From, To, FromToPath, FromToTime),
	FinalTime is FromTime + ToTime + FromToTime,
	reverse(FromPath, FromPathReverse),
	reverse(FromToPath, FromToPathReverse),
	append(FromPathReverse, FromToPathReverse, Temp),
	append(Temp, ToPath, FinalPath),
	InTaxi = Customers.
