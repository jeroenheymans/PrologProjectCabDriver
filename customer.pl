% Functions concerning the customers

% Takes the next customer to transport
%   NewCustomers = newly list of customers with first deleted
%   First = first customer formatted: TaxiTimeToDepart-CustomerID
nextCustomer([First|NewCustomers], First, NewCustomers).
    
% Calculate the distance from the parking lot to the 
% starting point of the customer. Used as preprocessing
%   CID = customer ID
%   Distance = time to get to customer from parking lot
distanceFromStartToCustomer(CID,Distance):-
    customer(CID, _, _, CustomerStart, _),
    startNode(ParkingLot),
    minimumDistance(ParkingLot, CustomerStart, _, Distance).
    
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
