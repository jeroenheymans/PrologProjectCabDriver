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
    
moveAllTaxis(CustomersToPickUp):-
    forall(transport(Taxi, Customers, NodeID, Distance, Path),
           (write('Taxi '),
            write(Taxi),
            writeln(' will be moved.'))).

followPath(0, [], _):-
    writeln('Finish!'),!.
    
followPath(0, Path, Current):-
    Path = [First|Rest],
    write('Taking next: '),
    writeln(First),
    edge(Current, First, Distance),
    followPath(Distance,Rest,First).
    
followPath(Distance, Path, Current):-
    NewDistance is Distance - 1,
    write('Distance still to do: '),
    writeln(NewDistance),
    followPath(NewDistance,Path,Current).

