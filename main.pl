% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions at first when compiling this file
%
% Execute in Prolog:
%   consult(main)
%   main(X).

:-['city.pl'].
:-['routeCalculation.pl'].
:-['customer.pl'].
:-['functions.pl'].

main(P):-
  distanceFromStart(3,P).
