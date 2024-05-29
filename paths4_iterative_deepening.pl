arc4(a,b).
arc4(b,c).
arc4(a,c).
arc4(b,d).


path4(X, Z, Path) :-
    length(Path, _),
    path_r(X, Z, Path).

path_r(Z, Z, []).
path_r(X, Z, [X|Path]) :-
    arc4(X, Y),
    path4(Y, Z, Path).