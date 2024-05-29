

%%MY ROSTERERING
%% 1 is am, 2 is pm, 0 is off....

ams_pms([1-2,2-1,0-_]).
days(5).
rr(R6):-
    % doc dd has rostter Day1 Day2 Day3 Day 4 etc
  
    days(/*constant*/DAYS),
    n_shifts_roster( DAYS , DD),
    n_shifts_roster( DAYS , EE),
    n_shifts_roster( DAYS , FF),
    n_shifts_roster( DAYS , GG),
    n_shifts_roster( DAYS , HH),
    n_shifts_roster( DAYS , JJ),
    n_shifts_roster( DAYS , KK),

    ams_pms(AMPM),
    label_and_gcc(DD,AMPM),
    label_and_gcc(EE,AMPM),
    label_and_gcc(FF,AMPM),
    label_and_gcc(GG,AMPM),
    label_and_gcc(HH,AMPM),
    label_and_gcc(JJ,AMPM),
    label_and_gcc(KK,AMPM),

    ROSTER=[DD,EE,FF,GG,HH,JJ,KK],

    transpose(ROSTER,TRANSPOSED),
    maplist(ampm_count2,TRANSPOSED),
    transpose(TRANSPOSED,R6).
  


%%%%HELPERS
no_consecutive_friday(ROSTER,SHIFT):-
    fridays(ROSTER,FRIDAYS),
    \+ adjacent(SHIFT,SHIFT,FRIDAYS).

mondays([],[]).
mondays([M,_,_,_,_,_,_|T],[M|T2]) :- 
    mondays(T,T2).

fridays([],[]).
fridays([_,_,_,_,_,F,_|T],[F|T2]):-
    fridays(T,T2).
    
saturdays([],[]).
saturdays([_,_,_,_,_,SAT,_|T],[SAT|T2]):-
    saturdays(T,T2).


adjacent(E0, E1, Es) :-
    append(_, [E0,E1|_], Es).

every_second(E0,E1,Es) :-
   append(_, [E0,_,E1|_], Es).

every_third(E0,E1,Es) :-
   append(_, [E0,_,_,E1|_], Es).




n_shifts_roster(NUM,ROSTER):-
    length(ROSTER,NUM),
    maplist(shift,ROSTER).

shift(X):-
    X in 0..2.
  
  
%%Vs = [_,_,_], global_cardinality(Vs, [1-2,3-_]), label(Vs).

label_and_gcc(LIST,GCC):-
    label(LIST),
    global_cardinality(LIST,GCC).

count_if(LIST,ITEM,RESULT):-
    include(=(ITEM),LIST,TEMP),
    length(TEMP,RESULT).

am_count(LIST):-
    count_if(LIST,1,R),
    R #>0,
    R #<5.

pm_count(LIST):-
    count_if(LIST,2,R),
    R #>0,
    R #<5.

ampm_count2(LIST):-
    global_cardinality(LIST,[1-X,2-Y,0-_]),
    X #>0,
    X #<5,
    Y #>0,
    Y #<5.

ampm_count(LIST):-
    count_if(LIST,1,AM),
    count_if(LIST,2,PM),
    AM #>0,
    PM #>0,
    AM #<5,
    PM #<5.

:- op(1192,fy,([define,def,week])).
:- op(1185,xfy,(array)).

%:- dynamic user:term_expansion/2.

term_expansion((week DOC array D1,D2,D3,D4,D5,D6,D7),(wk(DOC,[D1,D2,D3,D4,D5,D6,D7]))).    


week gm array 1,1,1,1,1,1,1.

cc([2,0,1,1,1, 0,0]).
cc([2,0,1,1,1, 0,0]).
cc([1,0,1,1,2, 0,0]).
cc([1,1,1,0,2, 0,0]).
cc([0,1,1,0,2, 0,0]).
cc([2,0,1,1,0, 1,1]).
cc([2,0,1,1,0, 2,2]).

x_week_roster(DOC,ROSTER):-
    findall(C,call(DOC,C),CC),findall([W1,W2],(member(W1,CC),member(W2,CC)),ROSTER).

xx(DOC,R):-
    findall(X,call(DOC,X),R).
  

%%?-findall(C,cc(C),CC),findall([W1,W2],(member(W1,CC),member(W2,CC)),CCW),member(W,CCW).
