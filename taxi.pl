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
    
putCustomersInTaxi(Customers, Taxi):-
    % append later on as optimization
    retract(transport(Taxi, _, NodeID, FinishID, Distance, Path)),
    assert(transport(Taxi, Customers, NodeID, FinishID, Distance, Path)).
    
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

getTaxisInTransport(Taxis):-
    findall(Taxi,
          (transport(TaxiID, _, _, _, _, _),
           Taxi = TaxiID),
          Taxis).   
          
dropOffCustomers([], _, _).

dropOffCustomers([Customer|Customers], NodeID, _):-
    customer(Customer, _, _, _, NodeID),
    write('Dropped off customer '),writeln(Customer),
    dropOffCustomers(Customers, NodeID, _).
    
getCustomersToPickUp(NodeID, PickUpCustomers):-
    findall(Customer,
            (customer(CID, _, _, NodeID, _),
             Customer = CID),
            PickUpCustomers).
          
% Reached finish
moveTaxi(TaxiID, Customers, 1, FinishID, [], NewDistance, NewNextNodeID, NewPath, PickUpCustomers):-
    dropOffCustomers(Customers, FinishID, _),
    getCustomersToPickUp(FinishID, PickUpCustomers),
    write('Taxi '),write(TaxiID),write(' reached destination and picks up: '),writeln(PickUpCustomers),
    moveTaxiContinue(PickUpCustomers, FinishID, NewNextNodeID, NewDistance, NewPath),
    write('Taxi '),write(TaxiID),write(' will ride to: '),writeln(NewNextNodeID).

% moveTaxi(TaxiID, Customers, Distance, NextNodeID, Path, NewDistance, NewNextNodeID, NewPath, NewCustomers)
moveTaxi(_, Customers, 1, NodeID, [Top|Rest], NewDistance, Top, Rest, Customers):-
    edge(NodeID,Top,NewDistance).
          
moveTaxi(_, Customers, Distance, NodeID, Path, NewDistance, NodeID, Path, Customers):-
    NewDistance is Distance - 1.  
            
moveTaxiContinue([], StartID, NewNextNodeID, NewDistance, NewPath):-
    writeln('Need to put more stuff here').

moveTaxiContinue([Customer|_], StartID, NewNextNodeID, NewDistance, NewPath):-
    customer(Customer,_,_,_,Destination),
    minimumDistance(StartID, Destination, Path, Distance),
    Path = [StartID|TempPath],
    TempPath = [NewNextNodeID|NewPath],
    edge(FinishID, NewNextNodeID, NewDistance),
    write('Sending taxi to node '),writeln(NewNextNodeID).
            
%moveTaxiContinue([Customer], NewNextNodeID, NewDistance, NewPath):-
%    writeln('Calling'),
%    customer(Customer,_,_,_,Destination),
%    minimumDistance(FinishID, Destination, Path, Distance),
%    Path = [FinishID|TempPath],
%    TempPath = [NewNextNodeID|NewPath],
%    edge(FinishID, NewNextNodeID, NewDistance).
          
moveTaxis([]).
          
moveTaxis([Taxi|Taxis]):-
    retract(transport(Taxi, Customers, NodeID, FinishID, Distance, Path)),
    moveTaxi(Taxi, Customers, Distance, NodeID, Path, NewDistance, NewNodeID, NewPath, NewCustomers),
    assert(transport(Taxi, NewCustomers, NewNodeID, FinishID, NewDistance, NewPath)),
    moveTaxis(Taxis).

% init for followpath:
%startTaxi(_, [First|Path]):-
%    writeln('Starting taxi'),
%    Path = [Second|Rest],
%    edge(First,Second,Distance),
%    followPath(Distance,Rest,Second,_,_,_).
startTaxi(Taxi, WhereTo, [First|Path]):-
    %\+transport(Taxi,_,_,_,_,_),
    Path = [Second|Rest],
    edge(First, Second, Distance),
    assert(transport(Taxi, [], Second, WhereTo, Distance, Rest)).
    %printStartTaxi(First, Second, Distance, Rest, WhereTo).

% Taxi has reached it destination
followPath(0, [], Current, 0, _, Current):-
    writeln('Finish!'),!.
    
% Taxi has reached a node
followPath(0, Path, Current, Distance, Rest, First):-
    Path = [First|Rest],
    writeln(First),
    edge(Current, First, Distance).
    
% Taxi is not yet at a new node
followPath(Distance, Path, Current, NewDistance, Path, Current):-
    NewDistance is Distance - 1,
    writeln(NewDistance).

sendTaxisToCustomers([]).

sendTaxisToCustomers([Customer|RestCustomers]):-
    pickEmptyTaxi(Taxi),
    customer(Customer, _, _, StartID, _),
    startNode(NodeID),
    minimumDistance(NodeID, StartID, Path, _),
    startTaxi(Taxi, StartID, Path),
    sendTaxisToCustomers(RestCustomers).
