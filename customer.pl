% Functions concerning the customers

% Takes the next customer to transport
% Is not yet completely correct as it only takes the first one
% and doesn't track already transported customers.
% TODO: complete
%   First = first customer
nextCustomer(First):-
    findall(ETOP-CID, customer(CID,ETOP, _,_,_), Result),
    keysort(Result,[First|_]).
    
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
              Customer=CID2),
            Customers).
