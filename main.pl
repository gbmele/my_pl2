% To run main, run the Repl and type 'main.' into the prom
:- include("nested_lists.pl").
write_me(X):- write(X).

st([write_me,"hit there how are you"]).

:- op(999, xfx, if).  
:- op(998, xfx, then). 


set(X):-
  X =.. [data,1,2,3].

trav([],[]).
trav([H|T],[H|T2]):-
  write("here is  "),write(H),nl,
  trav(T,T2).
  
  nonmember(X,[]).
  nonmember(X,[Y|Ys]):- dif(X,Y), nonmember(X,Ys). 
  
get(Code) :- repeat, get_char(Code), !.

%re_replace(+Pattern, +With, +String, -NewString))



:- dynamic pt/3.
pt(1,1,1).

poke(X,Y,VAL):-
  retract(pt(X,Y,_)),
  assert(pt(X,Y,VAL)).
peek(X,Y,VAL):-
  pt(X,Y,VAL).
  


block_if(cond(C),then(T),else(E)):-
  cond(C) -> call(T) ; call(E).


play:-
 read(X),
 write(X).

 process:-
 repeat,
   get_char(Term),
   (  Term == q
   -> !
   ;  write(Term),
     fail
   ).

:- use_module(library(settings)).
setting(version,"Current version").
setting(day_one,333).

%% SET LIST COL VAL


nth1_get(INDEX,LIST,RESULT):-
  nth1(INDEX,LIST,RESULT).

nth1_set(INDEX,LIST,VAL,RESULT):-
    nth1(INDEX,LIST,_,REST),
    nth1(INDEX,RESULT,VAL,REST).
    
%% OP_IN_A1_A2_etc_RESULT
 set_list_col_val(LIST,INDEX,VAL,RESULT):-
   nth1(INDEX,LIST,_,REST),
   nth1(INDEX,RESULT,VAL,REST).


nth1_nested_get(ROW,COL,NESTED,RESULT):-
  nth1(ROW,NESTED,WORKINGROW,REST),
  nth1(COL,WORKINGROW,RESULT).

nth1_nested_set(ROW,COL,NESTED,VAL,RESULT):-
  nth1(ROW,NESTED,WORKINGROW,REST),
  nth1_set(COL,WORKINGROW,VAL,NEWROW),
  nth1(ROW,RESULT,NEWROW,REST).


%% SET NESTED ROW COL VAL 
pp(IN_NESTED,ROW,COL,VAL,OUT_RESULT):-
  nth1(ROW,IN_NESTED,WORKINGROW,REST),
  set_list_col_val(WORKINGROW,COL,VAL,NEWROW),
  nth1(ROW,OUT_RESULT,NEWROW,REST).




  roster([[gm,a,p,o],[db,c,p,o]]).
  %%ROSTER is nested list of lists each list is [doc,shifts,shift,shift,shift] etc,
  get_doc_day_shift(ROSTER,DOC,DAY_NUM,RESULT):-
    nth1(_,ROSTER,[DOC|SHIFTS]),
    nth1(DAY_NUM,SHIFTS,RESULT).


    %% ?- roster(R),nth1(THEROW,R,[gm|SHIFTS],REST),set_list_col_val(SHIFTS,1,c,RR),C=[gm|RR],nth1(THEROW,FINAL,C,REST).
  
set_doc_day_shift(ROSTER,DOC,DAY_NUM,SH,RESULT):-
  nth1(THEROW,ROSTER,[DOC|SHIFTS],REST_ROSTER),
  set_list_col_val(SHIFTS,DAY_NUM,SH,RR),
  nth1(THEROW,RESULT,[DOC|RR],REST_ROSTER).


  nested([[1,2,33],[5,6,7],[99,55,44]]).

  t1(RESULT):-
    nested(N),
    (pp(N,1,1,777,R)  ; write(["fail",row,1,col,31,val,777])) ,
    pp(R,2,2,888,RR),
    pp(RR,3,3,999,RESULT).

revs([]) --> [].
revs([H|T]) --> revs(T),[H].
revs(L,R):- revs(L,R,[]). 

do_revs(L,R):- phrase(revs(L),R).
%%%
%%%

request(["qq",o,a,"A","a","am","MH0900-1800(9:00)","MH0800-1800(10:00)","MH0900-1730(9:50)", "p","pm","al","AL","cl","CL","lsl","LSL","LPPA","CONF","PAT","mat","ua","UA","up","UP","c","cs","CS"]).

get_sub(SH,a):-
  sub_string("a am A AM MH0900-1800(9:00) MH0800-1800(10:00) MH0900-1730(9:50)",_,_,_,SH).
get_sub(SH,p):-
 sub_string("p P PM pm MH1600-2400(8.00) MH1400-2400(8.00) MH1700-2400(8.00) 14:00-23:59",_,_,_,SH).
get_sub(SH,al):-
  sub_string("al AL AL(0:00) AL (0:00) AL(10:00)",_,_,_,SH).
get_sub(SH,c):-
sub_string("c cs C CS",_,_,_,SH).
get_sub(SH,cl):-
  sub_string("cl CL CONF CONF(0:00) CONF (0:00) CONF(10:00) CONF (10:00)",_,_,_,SH).
get_sub(SH,lsl):-
  sub_string("lsl LSL LPPA ",_,_,_,SH).
get_sub(SH,mat):-
  sub_string("mat MAT",_,_,_,SH).
get_sub(SH,pat):-
  sub_string("pat PAT",_,_,_,SH).
get_sub(SH,ua):-
  sub_string("uaUA",_,_,_,SH).
get_sub(SH,up):-
  sub_string("upUP",_,_,_,SH).
get_sub(SH,u):-
  sub_string("u U U00:23:59 U 00-23:59",_,_,_,SH).
get_sub(SH,o):-
  sub_string("oOoffOFF",_,_,_,SH).
get_sub(SH,x):-
  sub_string("xss",_,_,_,SH).

get_sub(_,"EEEEEERRRRRRROOOOOOOOORRRRRRR").

%%ts=test_substrings  
ts(R,RR):- 
 request(R),
 maplist(get_sub,R,RR).
 
 writeln("GG").