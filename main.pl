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
    
main(_):-
	writeln('New main').
