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
