:- op(1200, xfx, ==>).
:- op(1000, xfy, /\).
:- op(1100, xfy, \/).

term_expansion(A ==> B, B:- A).
goal_expansion(A /\ B, (A, B)).
goal_expansion(A \/ B, (A; B)).

man(X) /\ unmarried(X) ==> bachelor(X).

man(john).
man(peter).
unmarried(john).

main:-bachelor(X), writeln(X), nl, fail.
