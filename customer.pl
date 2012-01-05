% Functions concerning the customers

getAllCustomers(Customers):-
	findall(Customer,
			(customer(CID,ETOP,_,_,_),
			 Customer = ETOP-CID),
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
    
getCustomersOnNode([], _, [], []).   
 
% Get all the customers standing on a specific node
getCustomersOnNode([Customer|Customers], Node, [Customer|OnNode], NotOnNode):-
	customer(Customer, _, _, Node, _),
	getCustomersOnNode(Customers, Node, OnNode, NotOnNode).
	
getCustomersOnNode([Customer|Customers], Node, OnNode, [Customer|NotOnNode]):-
	getCustomersOnNode(Customers, Node, OnNode, NotOnNode).	
			
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
	
	
% Picks next customer, is not yet the best customer, just the first one
% that looks interesting
pickNextCustomer(Time, Node, Customer, Path, NewTime):-
	customerAvailable(Customer),
	customer(Customer, ETOP, LTOP, CStartID, _),
	ETOP >= Time,
	minimumDistance(Node, CStartID, Path, PathTime),
	NewTime = Time + PathTime,
	NewTime =< LTOP.
	
% Get the customer with the lowest available ETOP value
getMinETOP(Customer, ETOP):-
	findall(Customer,
		(customerAvailable(CID),
		 customer(CID, CETOP, _, _, _),
		 Customer = CETOP-CID),
		Customers),
	keysort(Customers, NewCustomers),
	NewCustomers = [ETOP-Customer|_].
	
