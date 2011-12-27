% Functions concerning the customers

% Takes the next customer to transport
%   NewCustomers = newly list of customers with first deleted
%   First = first customer formatted: TaxiTimeToDepart-CustomerID
nextCustomer([CustomerPickup-CustomerID|NewCustomers], CustomerPickup-CustomerID, NewCustomers).
    
% Calculate the distance from the parking lot to the 
% starting point of the customer. Used as preprocessing
%   CID = customer ID
%   Distance = time to get to customer from parking lot
distanceFromStartToCustomer(CID,NewDistance):-
    customer(CID, _, _, CustomerStart, _),
    startNode(ParkingLot),
    minimumDistance(ParkingLot, CustomerStart, _, Distance),
    NewDistance is Distance - 1.
    
% Get list of all the customers that are going to the same
% destination as given customer.
%   CID1 = Customer ID of given customer
%   Customers = list of customers as result
getCustomersSameDestination(CID1, Customers):-
    customer(CID1,_,_,_,Destination),
    findall(Customer,
            ( customer(CID2,_,_,_,Destination),
              \+CID1=CID2,
              Customer=CID2 ),
            Customers).
            
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
    
% Get all the customers standing on a specific node
% TODO: filter with time
getCustomersOnNode(Node, Customers):-
	findall(Customer,
			( customer(CID, _, _, Node, _),
			  Customer = CID),
			Customers).
    
getBestCustomer([BestCustomer],NodeID,_,BestCustomer,BestDistance,[]):-
	distanceFromNodeToCustomer(NodeID, BestCustomer, BestDistance).
    
getBestCustomer([Customer|Customers], NodeID, Time, BestCustomer, BestDistance, NewCustomers):-
	getBestCustomer(Customers, NodeID, Time, BestCustomer, BestDistance, OtherCustomers),
	distanceFromNodeToCustomer(NodeID, Customer, Distance),
	(Distance =< BestDistance
	-> (NewBestCustomer = Customer,
		NewBestDistance = Distance,
		append(OtherCustomers, [BestCustomer], NewCustomers))
	;  (NewBestCustomer = BestCustomer,
		NewBestDistance = BestDistance,
		append(OtherCustomers, [Customer], NewCustomers))).
	
distanceFromNodeToCustomer(NodeID, CID, NewDistance):-
    customer(CID, _, _, CustomerStart, _),
    minimumDistance(NodeID, CustomerStart, _, Distance),
    NewDistance is Distance - 1.
