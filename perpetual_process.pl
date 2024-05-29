%%%
%%% simple interactive

echo:- read(X),echo(X).

echo(X):- last_input(X),!.

echo(X):- write(X),nl,
          read(Y),!,
          echo(Y).

last_input(q):-
 write("thanks,done").


%%
%% PERPETUAL PROCESSES.
%%

st:- st([0]).
st(File):- 
 writeln(File),
 read(Command),
 st(File,Command).

st(File,exit):- 
    writeln("bye"),!.

st(File,Command):-
apply(Command,File,File1), !, st(File1).

apply(init,_,[0]).

apply(add_x(X),[OLDX|T],[NEWX|T]):-
  NEWX is OLDX + X.

apply(show_me,LIST1,LIST1):-
    writeln(LIST1).

apply(commands,LIST1,LIST1):-
    writeln("Commands-init,add_x(VAL),show_me").


 write_prompt:- write('»'), nl.