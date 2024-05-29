/*
switch(N) {
    case 1: procedure1(); break;
    case 2: procedure2(); break;
    default: procedure(); break;
    }
May be written as:

switch(1) :-
  procedure1.
switch(2) :-
  procedure2.
switch(_) :-
  procedure;
*/