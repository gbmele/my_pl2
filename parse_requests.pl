%%DATA

request(["qq",o,a,"A","a","am","MH0900-1800(9:00)","MH0800-1800(10:00)","MH0900-1730(9:50)", "p","pm","al","AL","cl","CL","lsl","LSL","LPPA","CONF","PAT","mat","ua","UA","up","UP","c","cs","CS","AP"]).


%%PREDS

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
get_sub(SH,ap):-
  sub_string("AP A00:23:59 A 00:23:59",_,_,_,SH).

get_sub(SH,o):-
  sub_string("oOoffOFF",_,_,_,SH).
get_sub(SH,x):-
  sub_string("xss",_,_,_,SH).

get_sub(_,"ERROR").

process_lines([DOC|SHIFTS],[UPDOC|TRANSLATED_SHIFTS]):-
  upcase_atom(DOC,UPDOC),
  maplist(get_sub,SHIFTS,TRANSLATED_SHIFTS),
  (nth1(DAY,TRANSLATED_SHIFTS,"ERROR") -> write("error shift "),write(UPDOC),write("-"),write(DAY),write("\n") ; true).

process_lines2([DOC|SHIFTS],[UPDOC|TRANSLATED_SHIFTS],[UPDOC,DAY,SHIFT]):-
  upcase_atom(DOC,UPDOC),
  maplist(get_sub,SHIFTS,TRANSLATED_SHIFTS),
  (nth1(DAY,TRANSLATED_SHIFTS,"ERROR") -> write("error shift "),write(UPDOC),write("-"),write(DAY),write("\n") ; true).


itemize(L,RESULT2):-
  reverse(L,REVERSED),
  itemize_(REVERSED,RESULT),
  reverse(RESULT,RESULT2).

itemize_([],[]).
itemize_([H|T],[item(H,CT)|T2]):-
  length(T,LL),
  CT is LL + 1,
  itemize_(T,T2).

itemise_and_count(IN,OUT):-
  itemise_and_count(IN,OUT,0).
itemise_and_count([],[],COUNT).
itemise_and_count([H|T],[item(CT,H)|T2],C):-
  CT is C + 1,
  itemise_and_count(T,T2,CT).
  


%%HELPERS
mapwrite(L):- maplist(write,L).
req([[gm,"MH0900-1800(9:00)",pm,o],[db,a,cl,lsl]]).


%%TESTS

%%ts=test_substrings  
ts(R,RR):- 
 req(REQUESTS),
 maplist(process_lines,REQUESTS,RR).