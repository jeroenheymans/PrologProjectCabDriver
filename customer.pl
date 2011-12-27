% Functions concerning the customers
    
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
    
% Get all the customers standing on a specific node
% TODO: filter with time
getCustomersOnNode(Node, Customers):-
	findall(Customer,
			( customer(CID, _, _, Node, _),
			  Customer = CID),
			Customers).
			
distanceFromNodeToCustomers(NodeID, Time, [Customer], [NewTime-Customer]):-
	distanceFromNodeToCustomer(NodeID, Customer, NewTime).
			
distanceFromNodeToCustomers(NodeID, Time, [Customer|Customers], ResultCustomers):-
	distanceFromNodeToCustomers(NodeID, Time, Customers, NewCustomers),
	distanceFromNodeToCustomer(NodeID, Customer, NewTime),
	append(NewCustomers, [NewTime-Customer], ResultCustomers).
	
getBestCustomer(Customers, NodeID, Time, BestCustomer):-
	distanceFromNodeToCustomers(NodeID, Time, Customers, CustomersTime),
	getBestCustomerInner(Time, CustomersTime, BestCustomer).
	
getBestCustomerInner(_, [], []).
	
getBestCustomerInner(Time, [CTime-CID|Rest], BestCustomer):-
	customer(CID, ETOP, LTOP, _, _),
	((ETOP=<Time+CTime,Time+CTime=<LTOP)
	 -> BestCustomer = CID
	 ; getBestCustomerInner(Time, Rest, BestCustomer)).	
	
distanceFromNodeToCustomer(NodeID, CID, NewDistance):-
    customer(CID, _, _, CustomerStart, _),
    minimumDistance(NodeID, CustomerStart, _, Distance),
    NewDistance is Distance - 1.
