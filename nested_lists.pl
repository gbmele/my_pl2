tnl:- test_nested_lists.

test(SUMMARY,PRED):-
  write(SUMMARY),
  call(PRED).

tests:-
 nested(N),
 test("this is nested",                (nested_test(NESTED),write(NESTED))),
 test("get row 1 col 1 --->" ,         (nested_row_col_get(N,1,1,R),writeln(R))),
 test("set row 1 col 3 to val 44  --> ",(nested_row_col_val_set(N,1,3,44,R2),writeln(R2))).

test_nested_lists:-
  
  write("set row 2 col 2 555  --->  "), nested_row_col_val_set(NESTED,2,2,555,R3),
    writeln(R3),
  writeln("TESTING ROSTER_"),
  writeln("A ROSTER is a LIST OF [DOC|SHIFTS]"),
  write(NESTED), write(" roster gm 1 shift get--> "), roster_doc_day_shift_get(NESTED,gm,1,R4),
    writeln(R4),

  write(NESTED),write(" roster db 2 p set  --> " ), roster_doc_day_shift_set(NESTED,db,2,p,R5),
    writeln(R5),

  write("END TEST").  
% END_TEST 


nested_test([[gm,a,s,o,p],[db,c,c,o,a]]).

nested_row_col_get(NESTED,ROW,COL,RESULT):-
  nth1(ROW,NESTED,WORKINGROW,REST),
  nth1(COL,WORKINGROW,RESULT).

nested_row_col_val_set(NESTED,ROW,COL,VAL,RESULT):-
  nth1(ROW,NESTED,WORKINGROW,REST),
  nth1_set(COL,WORKINGROW,VAL,NEWROW),
  nth1(ROW,RESULT,NEWROW,REST).

%%ROSTER
roster([[gm,a,p,o],[db,c,p,o]]).

roster_doc_day_shift_get(ROSTER,DOC,DAY_NUM,RESULT):-
  nth1(_,ROSTER,[DOC|SHIFTS]),
  nth1(DAY_NUM,SHIFTS,RESULT).

roster_doc_day_shift_set(ROSTER,DOC,DAY_NUM,SH,RESULT):-
  nth1(THEROW,ROSTER,[DOC|SHIFTS],REST_ROSTER),
  list_col_val_set(SHIFTS,DAY_NUM,SH,RR),
  nth1(THEROW,RESULT,[DOC|RR],REST_ROSTER).

list_col_val_set(LIST,INDEX,VAL,RESULT):-
 nth1(INDEX,LIST,_,REST),
 nth1(INDEX,RESULT,VAL,REST).
