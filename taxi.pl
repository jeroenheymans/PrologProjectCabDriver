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
           (followPath(Distance, Path, NodeID, NewDistance, NewPath, NewNodeID),
            write('Taxi '),
            write(Taxi),
            write(' has distance to do: '),
            writeln(NewDistance))).

% init for followpath:
startTaxi(Taxi, [First|Path]):-
    Path = [Second|Rest],
    edge(First,Second,Distance),
    followPath(Distance,Rest,Second).

% Taxi has reached it destination
followPath(0, [], _, 0, _, _):-
    writeln('Finish!'),!.
    
% Taxi has reached a node
followPath(0, Path, Current, Distance, Rest, First):-
    Path = [First|Rest],
    write('Taking next: '),
    writeln(First),
    edge(Current, First, Distance).
    
% Taxi is not yet at a new node
followPath(Distance, Path, Current, NewDistance, Path, Current):-
    NewDistance is Distance - 1,
    write('Distance still to do: '),
    writeln(NewDistance).

