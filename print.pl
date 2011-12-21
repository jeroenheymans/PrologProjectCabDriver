printNewCustomerInTaxi(CustomerID,Taxi,NewClock):- 
    write('Put customer '),
    write(CustomerID),
    write(' in taxi '),
    write(Taxi),
    write(' at time '),
    writeln(NewClock).
    
printStartTaxi(First, Second, Distance, Path, WhereTo):-
    write('Started taxi. Start at '),
    write(First),
    write(', now going to '),
    write(Second),
    write(' (distance='),
    write(Distance),
    %write(') and path to follow: '),
    %write(Path),
    %write(' to end: '),
    %writeln(WhereTo).
    writeln(')').
