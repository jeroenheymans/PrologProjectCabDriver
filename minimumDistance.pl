% Contains all the functions concerning the calculation
% of the minimum distance between two nodes. edge/3 must
% already be defined when calling this!
%
% minimumDistance/4 is the only one that you need to 
% calculate the minimum distance.
%
% Example use:
%   ?- minimumDistance(1, 2, P, L).

% Calculate the minimum distance
%   Start = Start Node ID
%   Finish = Finish Node ID
%   ShortestPath = the found path
%   Length = the Length of the found path
% It calls dijkstra algorithm. 
minimumDistance(Start, Finish, ShortestPath, Length) :-
  dijkstra([0-[Start]], Finish, RestShort, Length),
  reverse(RestShort, ShortestPath).

% Dijkstra algorithm. We have reached the final so it is time to stop
%   Length = total Length already traveled
%   Finish = last node visited
%   RestPath = previous nodes visited
dijkstra([Length-[Finish|RestPath]|_], Finish, [Finish|RestPath], Length) :- !.

% Find best candidate and continue with the algorithm
%   Visited = already discovered path
%   Finish = end node
%   RestShortestPath = rest of the already discovered shortest path
%   Length = total Length of RestShortestPath
dijkstra(Visited, Finish, RestShortestPath, Length) :-
  bestCandidate(Visited, BestCandidate), 
  dijkstra([BestCandidate|Visited], Finish, RestShortestPath, Length).

% Search for the best candidate by performing a
% findall and then take the minimum (the one with less distance)
%   Paths = list of previously visited nodes (top = most recent)
%   BestCandidate = best node to take as next
bestCandidate(Paths, BestCandidate) :-
  findall(NewNode,                      % format in which the elements of the list must be formatted
    ( member(Length-[Node1|Path], Paths),  % take a member from the paths
      edge(Node1,Node2,Distance),       % take the edges where P1 starts from
      \+isVisited(Paths, Node2),        % we only want non-visited P2's
      NewLength is Length+Distance,           % we have an unvisited P2, we calculate total distance
      NewNode=NewLength-[Node2,Node1|Path] % we make new node in list
    ),
    Candidates                          % final list of all found candidates
  ),
  minimum(Candidates, BestCandidate).   % we take the minimum we have found

% Sort the list of candidates and only take the head
% the head is the minimum
%   Candidates = list of possible candidates to visit
%   BestCandidate = best candidate
minimum(Candidates, BestCandidate) :-
  keysort(Candidates, [BestCandidate|_]).

% Check if a node is already visited in the Paths
%   Paths = visited path
%   Node = node that could be member of the path
isVisited(Paths, Node) :-
  memberchk(_-[Node|_], Paths).
