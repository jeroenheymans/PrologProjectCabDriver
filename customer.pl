% Functions concerning the customers

getAllCustomers(Customers):-
	findall(Customer,
			(customer(CID,ETOP,_,_,_),
			 Customer = ETOP-CID),
			Customers).
			
getAllAvailableCustomers(Customers):-
	findall(Customer,
			(customerAvailable(CID, waiting, _, _),
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
	
neighbor(Node1, Node2):-edge(Node1, Node2, _).
neighbor(Node1, Node2):-neighbor(Node1, Node3),edge(Node3, Node2,_).

searchBf(Nodes, 0, Nodes):-!.
searchBf([Current|Rest], Ctr, Result):-
	NewCtr is Ctr - 1,
	findall(E, (edge(Current, Neighbor, _), \+member(Neighbor, Rest), E = Neighbor), Neighbors),
	append(Rest, Neighbors, Temp),
	append(Temp, [Current], NewSearchspace),
	searchBf(NewSearchspace, NewCtr, Result).
	
getNeighborhoodCustomers(Node, Time, Customers):-
	searchBf([Node], 2000, Nodes),
	findall(Customer,
		(member(CStartID, Nodes),
		customerAvailable(CID, waiting, _, _),
		customer(CID, ETOP, LTOP, CStartID, _),
		ETOP >= Time,
		minimumDistance(Node, CStartID, Path, PathTime),
		NewTime is Time + PathTime,
		NewTime =< LTOP,
		maximum(NewTime, ETOP, NewNewTime),
		Customer = NewNewTime-[CID-Path]),
		Customers).
	
% Picks next customer, is not yet the best customer, just the first one
% that looks interesting
% 	+Time = current time at which the taxi can start 
%	+Node = node where taxi is
%	-Customer = the next customer to pick
%	-Path = the path to this customer
%	-NewTime = time it takes to get to the customer
pickNextCustomer(Time, Node, CustomersSorted):-
	getNeighborhoodCustomers(Node, Time, Customers),
	keysort(Customers, CustomersSorted).
	
% Get the customer with the lowest available ETOP value
%	-Customer = the best customer
%	-ETOP = customer his ETOP
getMinETOP(Customer, ETOP):-
	findall(Customer,
		(customerAvailable(CID, waiting, _, _),
		 customer(CID, CETOP, _, _, _),
		 Customer = CETOP-CID),
		Customers),
	keysort(Customers, NewCustomers),
	NewCustomers = [ETOP-Customer|_].
	
% Set all customers that we know as available
setAllCustomersAvailable:-
	startNode(Start),
	findall(Customer,
		(customer(CID, ETOP, LTOP, Begin, Dest),
		 minimumDistance(Begin, Dest, Path1, Time1),
		 minimumDistance(Dest, Start, Path2, Time2),
		 ETOP + Time1 + Time2 =< 1440,
		 reverse(Path2, Temp),
		 assert(customerAvailable(CID, waiting, [Path1-Time1], [Temp-Time2])),
		 Customer = CID),
		 _).
		 
% Inner function for orderClosestCustomers/3
% stopcondition
orderClosestCustomersInner([], _, []).

% Inner function for orderClosestCustomers/3
%	+Customer = current customer
%	+Customers = other customers still to calculate
%	+Node = node where we are now
%	-Distance = distance to dropoff from Customer starting from Node
%	-NewCustomers = calculated customers
orderClosestCustomersInner([Customer|Customers], Node, [Distance-Customer|NewCustomers]):-
	orderClosestCustomersInner(Customers, Node, NewCustomers),
	customer(Customer, _, _, _, CNode),
	minimumDistance(Node, CNode, _, Distance).	
		 
% Order to the closest customers
%	+Customers = customers to order
%	+Node = node where we are now
%	-NewCustomers = ordered customers
orderClosestCustomers(Customers, Node, NewCustomers):-
	orderClosestCustomersInner(Customers, Node, UnorderedCustomers),
	keysort(UnorderedCustomers, OrderedCustomers),
	removeKeys(OrderedCustomers, NoKeysOrderedCustomers),
	reverse(NoKeysOrderedCustomers, NewCustomers).

% Calculate route between customers, endcondition
%	+Customers = []
%	+Node = node where we are now
%	+Path = path we already followed
%	-NewPath = final path
%	+Time = time it already is
%	-NewTime = Time = final time
routeBetweenCustomers([], Node, Path, NewPath, Time, Time):-
	NewPath = [Node|Path].
	
% Calculate route between customers
% 	+Customer = customer to calculate his path
%	+Customers = other customers still to calculate
%	+Node = node where we are now
%	+Path = path we already followed
%	-EndPath = final path we have to follow
%	+Time = time it already is
%	-NewTime = final time necessary to follow EndPath
routeBetweenCustomers([Customer|Customers], Node, Path, EndPath, Time, NewTime):-
	customer(Customer, _, _, _, EndNode),
	minimumDistance(Node, EndNode, [NewNode|P], PathTime),
	append(P, Path, NewPath),
	TempTime is Time + PathTime,
	routeBetweenCustomers(Customers, NewNode, NewPath, EndPath, TempTime, NewTime).
		 
% Calculate the drop off path as soon as we picked up customers
% 	+Customer = customers to drop off
%	+Node = node where we are now
%	+Time = time it is now
%	-Path = path we take to drop off customers
%	-NewTime = time it takes to follow the Path
calculateDropOffPath(Customers, Node, Time, Path, NewTime):-
	orderClosestCustomers(Customers, Node, NewCustomers),
	routeBetweenCustomers(NewCustomers, Node, [], Path, Time, NewTime).
