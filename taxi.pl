% Contains taxi related functions

% transport(TaxiID, NodeID, [Customer1ID, ..., Customer4ID])
%   TaxiID = taxi that is doing the transport
%   CustumerXID = ID of the customers in the taxi
%   NodeID = current node where the taxi is at
%   FinishID = end node where the taxi is going to
%   Distance = distance to travel to next node
%   Path = remaining path to do
:- dynamic transport/6.

% Tries to find a taxi that is not yet on the move
%   -Taxi = ID of the found empty taxi
pickEmptyTaxi(Taxi):-
    taxi(Taxi),
    \+transport(Taxi,_,_,_,_,_).
    
% Take an exisiting taxi and put the customer in it
% LATER: change the taxi so more customers can fit!
%   +Customer = ID of the customer to put in taxi
%   +Taxi = ID of the taxi to put the customer in
putCustomerInTaxi(Customer,Taxi):-
    assert(transport(Taxi,Customer,_,_,_)).
    
%moveAllTaxis(_):-
%    forall(transport(Taxi, _, NodeID, Distance, Path),
%           (write('Moving taxi '),writeln(Taxi),
%            followPath(Distance, Path, NodeID, NewDistance, NewPath, [NewNodeID]),
%            retract(transport(Taxi,_,_,_,_)),
%            (NewPath = [] 
%             -> (writeln('Taxi dropped customer off'),
%                 startNode(StartID),
%                 write('At node '),write(NewNodeID),write(' and going to '),writeln(StartID),
%                 minimumDistance(NewNodeID,StartID,PathToStart,_),
%                 assert(transport(Taxi,[],NewNodeID,_,_)),
%                 startTaxi(Taxi, PathToStart))
%             ; true),
%            write('Taxi '),
%            write(Taxi),
%            write(' has distance to do: '),
%            writeln(NewDistance))).

% init for followpath:
%startTaxi(_, [First|Path]):-
%    writeln('Starting taxi'),
%    Path = [Second|Rest],
%    edge(First,Second,Distance),
%    followPath(Distance,Rest,Second,_,_,_).
startTaxi(Taxi, WhereTo, [First|Path]):-
    \+transport(Taxi,_,_,_,_,_),
    Path = [Second|Rest],
    edge(Frist, Second, Distance),
    assert(transport(Taxi, [], NodeID, WhereTo, Distance, Rest)),
    printStartTaxi(First, Second, Distance, Rest, WhereTo).

% Taxi has reached it destination
followPath(0, [], Current, 0, _, Current):-
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

sendTaxisToCustomers([]).

sendTaxisToCustomers([Customer|RestCustomers]):-
    pickEmptyTaxi(Taxi),
    customer(Customer, _, _, StartID, _),
    startNode(NodeID),
    minimumDistance(NodeID, StartID, Path, _),
    startTaxi(Taxi, StartID, Path),
    write('Sending taxi '),write(Taxi),write(' to pick up customer '),writeln(Customer),
    sendTaxisToCustomers(RestCustomers).
