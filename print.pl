printClock:-
    clock(C),
    write('['),write(C),write('] ').

printNewCustomerInTaxi(CustomerID,Taxi,NewClock):- 
    printClock,
    write('Put customer '),
    write(CustomerID),
    write(' in taxi '),
    write(Taxi),
    write(' at time '),
    writeln(NewClock).
    
printStartTaxi(First, Second, Distance, Path, WhereTo):-
    printClock,
    write('Started taxi. Start at '),
    write(First),
    write(', now going to '),
    write(Second),
    write(' (distance='),
    write(Distance),
    writeln(')').
    
printTimesUp(Customers):-
    printClock,
    writeln('Times up!'),
    printClock,
    write('Remaining customers: '),
    writeln(Customers),
    printClock,
    writeln('Taxis still in transport: '),
    forall(transport(TaxiID,_,_,_,_,_),
          (write(TaxiID),write(' '))).
    
printCustomersToPickUpNow(Customers):-
    printClock,
    write('Sending taxi\'s to customers: '),writeln(Customers).
    
printDropOffCustomers(Taxi, []):-
    printClock,
    write('Taxi '),write(Taxi),writeln(' reached destination, no customers to drop off').
    
printDropOffCustomers(Taxi, Customers):-
    printClock,
    write('Taxi '),write(Taxi),write(' drops off customers '),writeln(Customers).
    
printTaxiIsHome(Taxi):-
    printClock,
    write('Taxi '),write(Taxi),writeln(' says: "Honey I\'m home!"').
    
printPickUpCustomers(Taxi, []):-
    printClock,
    write('Taxi '),write(Taxi),writeln(' reached destination, no customers to pick up').
    
printPickUpCustomers(Taxi, Customers):-
    printClock,
    write('Taxi '),write(Taxi),write(' reached destination and picks up: '),writeln(Customers).
