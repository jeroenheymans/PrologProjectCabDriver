% Functions concerning the customers

nextCustomer(First):-
    findall(ETOP-CID, customer(CID,ETOP, _,_,_), Result),
    keysort(Result,[First|_]).
