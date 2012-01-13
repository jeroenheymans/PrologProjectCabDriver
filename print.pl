% print related functionality, self-explainatory

printTransportableCustomers(Customers):-
	listLength(Customers, TotalNrCustomers),
	write(TotalNrCustomers),writeln(' customers can be transported').
	
printNoTaxisLeft:-
	writeln('No taxis left').
	
printCustomersLeft(CustomersLeft):-
	listLength(CustomersLeft, Total),
	write('Customers left ('),write(Total),write('): '),writeln(CustomersLeft).
	
printCustomersInTaxi(Taxi, InTaxi):-
	write('Taxi '),write(Taxi),write(' will transport: '),writeln(InTaxi).
	
printTransportLoop([]).
		 
printTransportLoop([Taxi|Jobs]):-
	taxiJob(Taxi, Customers, Path, FinalTime),
	write('Route for taxi '),write(Taxi),write(' (arrival: '),write(FinalTime),writeln('):'),
	routeLoop(Customers, Path),
	printTransportLoop(Jobs).
	
routeLoop(_, []).
	
routeLoop(Customers, [Node|Path]):-
	customersToPickupOrDropoff(Customers, Node, OnNode),
	nodeLoop(Node, OnNode),
	routeLoop(Customers, Path).
	
nodeLoop(_, []).

nodeLoop(Node, [Customer|Customers]):-
	customer(Customer, _, _, Node, _),
	write('Picking up customer #'),writeln(Customer),
	nodeLoop(Node, Customers).
	
nodeLoop(Node, [Customer|Customers]):-
	customer(Customer, _, _, _, Node),
	write('Dropping off customer #'),writeln(Customer),
	nodeLoop(Node, Customers).
