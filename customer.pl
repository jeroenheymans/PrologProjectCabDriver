% Functions concerning the customers

getAllCustomers(Customers):-
	findall(Customer,
			(customer(CID,ETOP,_,_,_),
			 Customer = ETOP-CID),
			Customers).
			
getAllAvailableCustomers(Customers):-
	findall(Customer,
			(customerAvailable(CID),
			 Customer = CID),
			Customers).
    
% Calculate the distance from the parking lot to the 
% starting point of the customer. Used as preprocessing
%   CID = customer ID
%   Distance = time to get to customer from parking lot
distanceFromStartToCustomer(CID,NewDistance):-
    customer(CID, _, _, CustomerStart, _),
    startNode(ParkingLot),
    minimumDistance(ParkingLot, CustomerStart, _, Distance),
    NewDistance is Distance - 1.
            
% Calculate the departure times necessary for the taxi's to
% pickup the customers on time. For example, if the user wants
% to be picked up at 600 minutes (10am) and a taxi needs to drive
% 30 minutes to get to the customer, the taxi must leave at best
% at 570 (9.30am)
%   CustomersToPickUp = pairs of time to depart from depot and customer ID (unsorted!)
%   ETOP = Estimated Time Of Pickup
%   CID = Customer ID
%   Distance = Distance from start to customer
% Example result: [[1038-0], [233-1], [587-2], [51-6], [... - ...], [...]|...]
% with [DepartureTime - CustomerID]
getDeparturesForPickupCustomers(CustomersToPickUp):-
    findall(Customer,
            ( customer(CID,ETOP,_,_,_),
              distanceFromStartToCustomer(CID, Distance),
              NewDistance is ETOP - Distance,
              Customer = NewDistance-CID
             ),
            CustomersToPickUp).
    
% Stopcondition for customersToPickupNow        
% customersToPickupNow(NewClock, RemainingCustomers, CustomersNowToPickUp, CustomersToPickUpLater)
customersToPickupNow(_, [], [], []).

% Split the list in 2 parts.
% Here we put the current customer (head of RemainingCustomers) in
% the list of CustomersNowToPickUp as the leavingtime is equal to the clock
customersToPickupNow(Clock, [LeaveTime-CID|Remaining], [LeaveTime-CID|PickUpNow], PickUpLater):- 
    LeaveTime =< Clock, !, 
    customersToPickupNow(Clock, Remaining, PickUpNow, PickUpLater).
    
% Split the list in 2 parts.
% Here we put the current customer (head of RemainingCustomers) in
% the list of CustomersToPickUpLater as the leavingtime isn't equal to the clock
customersToPickupNow(Clock, [LeaveTime-CID|Remaining], PickUpNow, [LeaveTime-CID|PickUpLater]) :- 
    LeaveTime =\= Clock, 
    customersToPickupNow(Clock, Remaining, PickUpNow, PickUpLater).
    
getCustomersPickupOnNode([], _, [], []).   
 
% Get all the customers standing on a specific node
getCustomersPickupOnNode([Customer|Customers], Node, [Customer|OnNode], NotOnNode):-
	customer(Customer, _, _, Node, _),
	getCustomersPickupOnNode(Customers, Node, OnNode, NotOnNode).
	
getCustomersPickupOnNode([Customer|Customers], Node, OnNode, [Customer|NotOnNode]):-
	getCustomersPickupOnNode(Customers, Node, OnNode, NotOnNode).	
			
distanceFromNodeToCustomers(NodeID, _, [Customer], [NewTime-Customer]):-
	distanceFromNodeToCustomer(NodeID, Customer, NewTime).
			
distanceFromNodeToCustomers(NodeID, Time, [Customer|Customers], ResultCustomers):-
	distanceFromNodeToCustomers(NodeID, Time, Customers, NewCustomers),
	distanceFromNodeToCustomer(NodeID, Customer, NewTime),
	append(NewCustomers, [NewTime-Customer], ResultCustomers).
	
getBestCustomerOld(Customers, NodeID, Time, BestCustomer):-
	distanceFromNodeToCustomers(NodeID, Time, Customers, CustomersTime),
	getBestCustomerInner(Time, CustomersTime, BestCustomer).
	
getBestCustomerOldInner(_, [], []).
	
getBestCustomerOldInner(Time, [CTime-CID|Rest], BestCustomer):-
	customer(CID, ETOP, LTOP, _, _),
	((ETOP=<Time+CTime,Time+CTime=<LTOP)
	 -> BestCustomer = CID
	 ; getBestCustomerOldInner(Time, Rest, BestCustomer)).	
	
distanceFromNodeToCustomer(NodeID, CID, NewDistance):-
    customer(CID, _, _, CustomerStart, _),
    minimumDistance(NodeID, CustomerStart, _, Distance),
    NewDistance is Distance - 1.
    
getBestCustomer(Customers, NodeID, Time, BestCustomer, NewCustomers):-
	getBestCustomerInner(Customers, NodeID, Time, BestCustomer, [], NewCustomers).
    
getBestCustomerInner([], _, _, _, NewCustomers, NewCustomers).

getBestCustomerInner([Customer|RestCustomers], NodeID, Time, BestCustomer, Temp, NewCustomers):-
	customer(Customer, ETOP, LTOP, Start, _),
	minimumDistance(NodeID, Start, _, Distance),
	((ETOP=<Time+Distance,Time+Distance=<LTOP)
	 -> (BestCustomer = Customer,
	 	 append(Temp, RestCustomers, NewCustomers))
	 ;	getBestCustomerInner(RestCustomers, NodeID, Time, BestCustomer, [Customer|Temp], NewCustomers)).
	
	
getNeighborhoodCustomers(Node, Time, Customers):-
	findall(Customer,
		((edge(Node, CStartID, _);(edge(Node, X, _),edge(X,CStartID, _))),
		customer(CID, ETOP, LTOP, CStartID, _),
		customerAvailable(CID),
		ETOP >= Time,
		minimumDistance(Node, CStartID, Path, PathTime),
		NewTime is Time + PathTime,
		NewTime =< LTOP,
		Customer = NewTime-[CID-Path]),
		Customers).
	
% Picks next customer, is not yet the best customer, just the first one
% that looks interesting
% 	+Time = current time at which the taxi can start 
%	+Node = node where taxi is
%	-Customer = the next customer to pick
%	-Path = the path to this customer
%	-NewTime = time it takes to get to the customer
pickNextCustomer(Time, Node, Customer, Path, NewTime):-
	getNeighborhoodCustomers(Node, Time, Customers),
	keysort(Customers, [NewTime-[Customer-Path]|_]).
	
% Get the customer with the lowest available ETOP value
%	-Customer = the best customer
%	-ETOP = customer his ETOP
getMinETOP(Customer, ETOP):-
	findall(Customer,
		(customerAvailable(CID),
		 customer(CID, CETOP, _, _, _),
		 Customer = CETOP-CID),
		Customers),
	keysort(Customers, NewCustomers),
	NewCustomers = [ETOP-Customer|_].
	
