% Functions that don't really fit anywhere specifically

startNode(Id):-node(Id,25,25).

square(Number,Result):-Result is Number*Number.

% transport(TaxiID, NodeID, [Customer1ID, ..., Customer4ID])
%   TaxiID = taxi that is doing the transport
%   NodeID = current node where the taxi is at
%   CustumerXID = ID of the customer in the taxi
:- dynamic transport/3.

pickEmptyTaxi(Taxi):-
    taxi(Taxi),
    \+transport(Taxi,_,_).
