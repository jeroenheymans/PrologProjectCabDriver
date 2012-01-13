% Functions concerning the customers
			
% Self-explainatory
% 	-Customers = list of the available customers
getAllAvailableCustomers(Customers):-
	findall(Customer,
			(customerAvailable(CID, _, _),
			 Customer = CID),
			Customers).
			
% Stopcondition when there are no more customers to pick up or drop off
customersToPickupOrDropoff([], _, []).   
 
% Get all the customers standing on a specific node or that need to be dropped off there
% 	+Customer = customer to look at if it can be picked up or dropped off
%	+Customers = remaining customers to consider
%	+Node = the node where all the picking up and dropping off happens
%	-OnNode = list of all the customers on this node
customersToPickupOrDropoff([Customer|Customers], Node, [Customer|OnNode]):-
	(customer(Customer, _, _, Node, _);customer(Customer, _, _, _, Node)),!,
	customersToPickupOrDropoff(Customers, Node, OnNode).
	
% Gets called when the first customer has nothing to do with +Node
%	+Customers = remaining customers to consider
%	+Node = the node where all the picking up and dropping off happens
%	-OnNode = list of all the customers on this node
customersToPickupOrDropoff([_|Customers], Node, OnNode):-
	customersToPickupOrDropoff(Customers, Node, OnNode).	

% Stopcondition for searchBf/3
%	+Nodes = nodes that have been considered
%	+Ctr = 0 in this specific case, we have looked at the maximum amount of neighbors
searchBf(Nodes, 0, Nodes):-!.

% Search in a breadthfirst kind of way for neighbors
%	+Current = current node
%	+Rest = other nodes to consider
%	+Ctr = counter to limit the depth we are looking
%	-Result = nodes from the hood bro ;)
searchBf([Current|Rest], Ctr, Result):-
	NewCtr is Ctr - 1,
	findall(E, (edge(Current, Neighbor, _), \+member(Neighbor, Rest), E = Neighbor), Neighbors),
	append(Rest, Neighbors, Temp),
	append(Temp, [Current], NewSearchspace),
	searchBf(NewSearchspace, NewCtr, Result).
	
% Get all the customers to consider that are in the neighborhood
% 	+Node = node where to start searching
%	+Time = current time, necessary to see if a customer can be picked up correctly
%	-Customers = list of all the customers to consider
getNeighborhoodCustomers(Node, Time, Customers):-
	searchBf([Node], 2000, Nodes),
	findall(Customer,
		(member(CStartID, Nodes),
		customerAvailable(CID, _, _),
		customer(CID, ETOP, LTOP, CStartID, _),
		ETOP >= Time,
		minimumDistance(Node, CStartID, Path, PathTime),
		NewTime is Time + PathTime,
		NewTime =< LTOP,
		maximum(NewTime, ETOP, NewNewTime),
		Customer = NewNewTime-[CID-Path]),
		Customers).
	
% Picks next customer, is not yet the best customer, just the first one
% that looks interesting
% 	+Time = current time at which the taxi can start 
%	+Node = node where taxi is
%	-CustomersSorted = list of all the customers to consider, the "firster", the bettter
pickNextCustomer(Time, Node, CustomersSorted):-
	getNeighborhoodCustomers(Node, Time, Customers),
	keysort(Customers, CustomersSorted).
	
% Get the customer with the lowest available ETOP value
%	-Customer = the best customer
%	-ETOP = customer his ETOP
getMinETOP(Customer, ETOP):-
	findall(Customer,
		(customerAvailable(CID, _, _),
		 customer(CID, CETOP, _, _, _),
		 Customer = CETOP-CID),
		Customers),
	keysort(Customers, NewCustomers),
	NewCustomers = [ETOP-Customer|_].
	
% Set all customers that we know as available
% We filter with all the customers that are impossible to pick up and drop off on time
setAllCustomersAvailable:-
	startNode(Start),
	findall(Customer,
		(customer(CID, ETOP, _, Begin, Dest),
		 minimumDistance(Begin, Dest, Path1, Time1),
		 minimumDistance(Dest, Start, Path2, Time2),
		 ETOP + Time1 + Time2 =< 1440,
		 reverse(Path2, Temp),
		 assert(customerAvailable(CID, [Path1-Time1], [Temp-Time2])),
		 Customer = CID),
		 _).
		 
% Inner function for orderClosestCustomers/3
% stopcondition
orderClosestCustomersInner([], _, []).

% Inner function for orderClosestCustomers/3
%	+Customer = current customer
%	+Customers = other customers still to calculate
%	+Node = node where we are now
%	-Distance = distance to dropoff from Customer starting from Node
%	-NewCustomers = calculated customers
orderClosestCustomersInner([Customer|Customers], Node, [Distance-Customer|NewCustomers]):-
	orderClosestCustomersInner(Customers, Node, NewCustomers),
	customer(Customer, _, _, _, CNode),
	minimumDistance(Node, CNode, _, Distance).	
		 
% Order to the closest customers
%	+Customers = customers to order
%	+Node = node where we are now
%	-NewCustomers = ordered customers
orderClosestCustomers(Customers, Node, NewCustomers):-
	orderClosestCustomersInner(Customers, Node, UnorderedCustomers),
	keysort(UnorderedCustomers, OrderedCustomers),
	removeKeys(OrderedCustomers, NoKeysOrderedCustomers),
	reverse(NoKeysOrderedCustomers, NewCustomers).
