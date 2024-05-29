arc(1,2).
arc(2,3).
arc(3,4).
arc(3,5).
arc(2,5).
arc(5,6).
arc(2,6).

%%read this declartively quite well....
path(X,Y,[arc(X,Y)]) :- 
  arc(X,Y).
path(X,Y,[arc(X,Z)|P]) :- 
  arc(X,Z),
  path(Z,Y,P).

arc_cost(1,2,10).
arc_cost(2,3,22).
arc_cost(2,3,15).
arc_cost(3,4,5).
arc_cost(4,2,8).
arc_cost(3,5,4).

path_cost(X,Y,[arc_cost(X,Y,C)]) :- 
  arc_cost(X,Y,C).
path_cost(X,Y,[arc_cost(X,Z,C)|P]) :- 
  arc_cost(X,Z,C),
  path_cost(Z,Y,P).

path_arc_cost(X,Y,[arc_cost(X,Y,C)]) :- 
  arc_cost(X,Y,C).
path_arc_cost(X,Y,[arc_cost(X,Z,C)|P]) :- 
  arc_cost(X,Z,C),
  path_arc_cost(Z,Y,P).

path_arc_cost_running_sum.


len_list([],0).
len_list([H|T],C):-
  len_list(T,CT),
  C is CT + 1.


sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.

sum_cost([], 0).
sum_cost([arc_cost(_,_,H)|T], Sum) :-
  sum_cost(T, Rest),
  Sum is H + Rest.

%% by first principles
x([],[],0).
x([arc_cost(X,Y,Z)|T],[Z|T2],SUM):-
  x(T,T2,REST),
  SUM is Z + REST.

get_cost(arc_cost(_,_,C),C).

%%running_sum
run([], _, []).            % For an empty list, the running sum is irrelevant
run([X|T], S, [S1|ST]) :-  % For a list start with X, the new list has a new running sum (S1)
    S1 #= X + S,           % New running sum (S1) is the current running sum plus X
    run(T, S1, ST).        % recurse on the tails of the lists with the new running sum    

running([],[],_). 
running([H|T],[S1|T2],RUNNINGSUM):-
    S1 #= H + RUNNINGSUM,
    running(T,T2,S1).
    
graph1(G) :- G =
  [ a -> [b, e], b -> [c, f], c -> [],
    d -> [], e -> [d, f], f -> [c],
    g -> [d, h], h -> [f] ].

edge(G, V, W) :- member(V -> L, G), member(W, L).
