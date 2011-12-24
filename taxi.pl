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
    
% Take an exisiting taxi and put the customers in it
% LATER: change the taxi so more customers can fit!
%   +Customers = IDs of the customers to put in taxi
%   +Taxi = ID of the taxi to put the customers in    
putCustomersInTaxi(Customers, Taxi):-
    retract(transport(Taxi, _, NodeID, FinishID, Distance, Path)),
    assert(transport(Taxi, Customers, NodeID, FinishID, Distance, Path)).

% Get all the taxi's that are currently transporting
%   -Taxis = all the taxi ID's
getTaxisInTransport(Taxis):-
    findall(Taxi,
          (transport(TaxiID, _, _, _, _, _),
           Taxi = TaxiID),
          Taxis).   
          
% Endcondition for dropOffCustomers/3
dropOffCustomers([], _, _).

% Drop off every customer in the taxi
% TODO: keep track of the dropped off customers?
%   +Customer = customer ID to drop off
%   +Customers = still to drop off
%   +NodeID = the ID of the node where we drop off
%   +_ = unused, why??? TODO: fix this!
dropOffCustomers([Customer|Customers], NodeID, _):-
    customer(Customer, _, _, _, NodeID),
    write('Dropped off customer '),writeln(Customer),
    dropOffCustomers(Customers, NodeID, _).
    
% Get a list of all the customers that we can pick
% up at given node ID
%   +NodeID = ID of the node where we want to pick up
%   -PickUpCustomers = the list of customers to pick up
getCustomersToPickUp(NodeID, PickUpCustomers):-
    findall(Customer,
            (customer(CID, _, _, NodeID, _),
             Customer = CID),
            PickUpCustomers).
         
% Endcondition for when a taxi returns to the starting
% point. 
%   +TaxiID = ID of the taxi on the move
%   +Customers = customers in the taxi (should be empty? TODO!)
%   +NodeID = ID of the node where we are (should be 1275? TODO!)
%   +FinishID = 1275 (can we make this dynamic? TODO!)
%   +Distance = 1
%   +Path = []
moveTaxi(TaxiID, Customers, _, 1275, 1, []):- 
    write("Honey I'm home!, said Taxi "),writeln(TaxiID).
        
% Reached finish and it is not the starting point
%   +TaxiID = ID of the taxi on the move
%   +Customers = customers in the taxi
%   +NodeID = ID of the node where we are
%   +FinishID = ID of the node where we finish
%   +Distance = 1
%   +Path = []
moveTaxi(TaxiID, Customers, _, FinishID, 1, []):-
    dropOffCustomers(Customers, FinishID, _),
    write('Taxi '),write(TaxiID),write(' dropped off: '),writeln(Customers),
    getCustomersToPickUp(FinishID, PickUpCustomers),
    write('Taxi '),write(TaxiID),write(' reached destination and picks up: '),writeln(PickUpCustomers),
    moveTaxiContinue(PickUpCustomers, FinishID, NewNextNodeID, NewDistance, NewPath, NewFinishID),
    write('Taxi '),write(TaxiID),write(' will ride to: '),writeln(NewNextNodeID),
    assert(transport(TaxiID, PickUpCustomers, NewNextNodeID, NewFinishID, NewDistance, NewPath)).

% Reached new node and this is not yet the finish
%   +TaxiID = ID of the taxi on the move
%   +Customers = customers in the taxi
%   +NodeID = ID of the node where we are
%   +FinishID = ID of the node where we finish
%   +Distance = 1
%   +Path = []
moveTaxi(TaxiID, Customers, NodeID, FinishID, 1, [NextNodeID|NewPath]):-
    edge(NodeID,NextNodeID,NewDistance),
    assert(transport(TaxiID, Customers, NextNodeID, FinishID, NewDistance, NewPath)).
          
% Between two nodes
%   +TaxiID = ID of the taxi on the move
%   +Customers = customers in the taxi
%   +NodeID = ID of the node where we are
%   +FinishID = ID of the node where we finish
%   +Distance = 1
%   +Path = []
moveTaxi(TaxiID, Customers, NodeID, FinishID, Distance, Path):-
    NewDistance is Distance - 1,
    assert(transport(TaxiID, Customers, NodeID, FinishID, NewDistance, Path)).  
            
% We arrived at the finish, dropped off everyone and there is nobody to pickup 
% here so we return to home
% TODO: change so that we could possibly wait here?
%   +Customers = []
%   +StartID = ID of the node where we are now
%   -NewNextNodeID = node where we go to first in the path
%   -NewDistance = distance to NewNextNodeID
%   -NewPath = the rest of the path to follow
%   -NewFinishID = the ID of the endpoint
moveTaxiContinue([], StartID, NewNextNodeID, NewDistance, NewPath, NewFinishID):-
    startNode(NewFinishID),
    minimumDistance(StartID, NewFinishID, Path, Distance),
    Path = [StartID|TempPath],
    TempPath = [NewNextNodeID|NewPath],
    edge(StartID, NewNextNodeID, NewDistance).

% We arrived at the finish and are able to pickup a customer
%   +Customer = customer (first of the list) that we pick up
%   +StartID = ID of the node where we are now
%   -NewNextNodeID = node where we go to first in the path
%   -NewDistance = distance to NewNextNodeID
%   -NewPath = the rest of the path to follow
%   -NewFinishID = the ID of the endpoint
moveTaxiContinue([Customer|_], StartID, NewNextNodeID, NewDistance, NewPath, Destination):-
    customer(Customer,_,_,_,Destination),
    minimumDistance(StartID, Destination, Path, Distance),
    Path = [StartID|TempPath],
    TempPath = [NewNextNodeID|NewPath],
    edge(FinishID, NewNextNodeID, NewDistance).
            
% Endcondition to loop over all the taxis
moveTaxis([]).
          
% Loop over all the taxis to move them
%   +Taxi = taxi we move in this iteration
%   +Taxis = other taxis to move
moveTaxis([Taxi|Taxis]):-
    retract(transport(Taxi, Customers, NodeID, FinishID, Distance, Path)),
    moveTaxi(Taxi, Customers, NodeID, FinishID, Distance, Path),
    moveTaxis(Taxis).

startTaxi(Taxi, WhereTo, [First|Path]):-
    %\+transport(Taxi,_,_,_,_,_),
    Path = [Second|Rest],
    edge(First, Second, Distance),
    assert(transport(Taxi, [], Second, WhereTo, Distance, Rest)).
    %printStartTaxi(First, Second, Distance, Rest, WhereTo).

sendTaxisToCustomers([]).

sendTaxisToCustomers([Customer|RestCustomers]):-
    pickEmptyTaxi(Taxi),
    customer(Customer, _, _, StartID, _),
    startNode(NodeID),
    minimumDistance(NodeID, StartID, Path, _),
    startTaxi(Taxi, StartID, Path),
    sendTaxisToCustomers(RestCustomers).
