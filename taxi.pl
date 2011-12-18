% Contains taxi related functions

% transport(TaxiID, NodeID, [Customer1ID, ..., Customer4ID])
%   TaxiID = taxi that is doing the transport
%   CustumerXID = ID of the customer in the taxi
%   NodeID = current node where the taxi is at
%   Distance = distance to travel to next node
%   Path = remaining path to do
:- dynamic transport/5.

pickEmptyTaxi(Taxi):-
    taxi(Taxi),
    \+transport(Taxi,_,_,_,_).
    
moveAllTaxis(CustomersToPickUp).


