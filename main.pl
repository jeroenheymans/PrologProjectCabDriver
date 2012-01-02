% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main) or reconsult(main)
%   main.

% Necessary includes
%:-['city_smaller.pl'].
:-['city.pl'].
%:-['city_smallest.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['taxi.pl'].
:-['functions.pl'].
:-['print.pl'].

test:-
	customer(CID1, _, _, X, _),
	customer(CID2, _, _, Y, _),
	CID1 =\= CID2,
	X == Y,
	write(CID1),write('-'),writeln(CID2).
    
main(_):-
	loop(0,0).
	
loop(50,49):-
	writeln('Finish!').
	
loop(50, J):-
	NewJ is J + 1,
	loop(0, NewJ).

loop(I, J):-
	NewI is I + 1,
	((node(Node, I, J), customer(CID, _, _, Node, _))
	-> (write(I),write('-'),writeln(J))
	; true),
	loop(NewI, J).
	
