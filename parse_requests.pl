%%DATA

request(["sab","qq",o,a,"A","a","am","MH0900-1800(9:00)","MH0800-1800(10:00)","MH0900-1730(9:50)", "p","pm","al","AL","cl","CL","lsl","LSL","LPPA","CONF","PAT","mat","SAB","sab","ua","UA","up","UP","c","cs","CS","AP"]).


%%PREDS
% So as to not use the member funtion provided in some prolog libraries.
%% This makes the code self sufficient
member_of(X,[X|_]):- !.
member_of(X,[_|T]) :- member_of(X,T).



%get_sub(SH,a):-
%  member_of(SH,[a,am]).
get_sub(SH,a):-  
  sub_string("MH0900-1800(9:00) MH0800-1800(10:00) MH0900-1730(9:50) a am A AM",_,_,_,SH).
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
get_sub(SH,sab):-
  sub_string("SAB sab SABB",_,_,_,SH).
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

%%DUCKSNUTS
process_lines2([DOC|SHIFTS],[UPDOC|TRANSLATED_SHIFTS],ITEMISED):-
  upcase_atom(DOC,UPDOC),
  maplist(get_sub,SHIFTS,TRANSLATED_SHIFTS),
  findall(item(DOC,DAY,SH),nth1(DAY,TRANSLATED_SHIFTS,SH),ITEMISED),
  (nth1(DAY,TRANSLATED_SHIFTS,"ERROR") -> write("error shift "),write(UPDOC),write(" - day -"),write(DAY),write("\n") ; true).

  
ll([],0).
ll([H|T],C):-
  ll(T,CT),
  C is CT + 1.


%%HELPERS
mapwrite(L):- maplist(write,L).
req([[gm,a,p,o],[db,a,cl,lsl]]).


%%TESTS
tm:-
  test_test_data.

%%ts=test_substrings  
ts(R,RR,RRR):- 
 req(REQUESTS),
 maplist(process_lines2,REQUESTS,RR,RRR).

test_test_data:-
  request(R),maplist(get_sub,R,RR),maplist(write,RR).


ts2(REQUESTS,RR,I):-
    req(REQUESTS),
    pr(REQUESTS,RR,I).


employees([
 [1, 75, 0, 30, 25],
 [2, 83, 0, 45, 25],
 [3, 90, 1, 45, 50],
 [4, 45, 3, 75, 25],
 [5, 89, 0, 52, 50]
 ]).

ex2(EmployeeId,LastReviewScore) :-
 employees(Es),
 tuples_in([[EmployeeId, LastReviewScore, NumOfSafetyViolations, TimeInGrade, RequiredTimeForPromotion]], Es),
 %LastReviewScore #> 40, %%NumOfSafetyViolations #>= 1, %TimeInGrade #> RequiredTimeForPromotion,
 label([EmployeeId]).

%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%

%% just unfolded the maplist out of this --- 

%%USAGE pr===process_roster
% just get a nested list r--- requests(R)
% then pr(R,RR,ITEMISED)

pr([],[]).
pr([[DOC|SHIFTS]|REST_ROSTER],[FOUND_ALL|I2]):-
    (staff(DOC,EMPNUM) -> true ; write("NO DOC")),
    findall([RON_STRING_1,RON_STRING_2],
            (   nth1(DAY,SHIFTS,SHIFT),
                fixed_date(DAY,FIXED_DAY),
                (split_shifts(SHIFT,S1,S2) -> true 
                                            ; write_list(["NO SHIFT ",DOC,"DAY ",DAY,SHIFT]) ,S1=err,S2=err_noshift) ,
                (S1=off -> RON_STRING_1 = "" 
                         ; rosteron_string(DOC,datenum-DAY,EMPNUM,S1,RON_STRING_1)),
                (S2=off -> RON_STRING_2 = "" 
                         ; rosteron_string(DOC,datenum-DAY,EMPNUM,S2,RON_STRING_2))
            ), 
            FOUND_ALL),
    pr(REST_ROSTER,I2).

validate_shift.

gm_string_array_result(STRING,ARRAY,RESULT):-
   swritef(RESULT,STRING,ARRAY).
   
rosteron_string(FIRST,SECOND,THIRD,FOURTH,RESULT):-
    DAGEND="NNNN,,,NNN,,NNNNN,,,",
    gm_string_array_result("%w,%w,%w,%w,%w\n",[FIRST,SECOND,THIRD,FOURTH,DAGEND],RESULT).

staff(db,1111111).
staff(gm,2222222).

fixed_date(DAY,RESULT):-
  RESULT=[daynum-DAY,"day"].

split_shifts(a,a1,a2).
split_shifts(p,p1,p2).
split_shifts(cl,cl,off).
split_shifts(lsl,lsl,off).
split_shifts(o,off,off).

write_list([]):-
  writeln("").
  
write_list([H|T]):-
 write(H),write(" "),
 write_list(T).

