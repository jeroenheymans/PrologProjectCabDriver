% Main program
%
% Contains a main-function to execute the program
% Loads all the other functions
%
% Execute in Prolog:
%   ['program.pl'].
%   main(X).

main(P):-
  ['city.pl'],
  ['routeCalculation.pl'],
  ['customer.pl'],
  nextCustomer(P).
