edge(a, b).
edge(a, c).
edge(b, d).
edge(c, d).
edge(d, e).
edge(e, f).

path3(Start, End, Path).
path3(Node, Node, [Node]).

path3(Start, End, [Start|Path]) :- 
 edge(Start, Intermediate), 
 path3(Intermediate, End, Path).

path3(Start, End, Visited, Path) :-
  edge(Start, Intermediate),
  \+ member(Intermediate, Visited),
 path3(Intermediate, End, [Intermediate|Visited], Path).


%%%%%
%%%%%
 path4(Start,Finish) :- smart_path(Start,Finish,[]).

 smart_path(Current,Target,Visited) :- edge(Current,Target).

 smart_path(Current,Target,Visited) :- 
   edge(Current,Next),non_member(Next,Visited),
   smart_path(Next,Target,[Next|Visited]).

 non_member(Elt,[]).
 non_member(Elt,[Hd | Tl]) :- Elt \== Hd, non_member(Elt,Tl).
