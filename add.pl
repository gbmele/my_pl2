:- module(add, [add_all/2, add_some/3]).

add_all([H|T], S0) :-
  add_all(T, S1),
  S0 is H + S1.
add_all([],0).