make_grid(LIST,GRID):-
    findall(NUM-ITEM,nth0(NUM,LIST,ITEM),GRID).

row_col_grid(ROW,COL,NEW_VAL,GRID,NEWGRID):-
    ITEM is ROW * 3 + COL,
    nth0(ITEM,GRID,ITEM-VAL,REST),
    nth0(ITEM,NEWGRID,ITEM-NEW_VAL,REST).

write_grid([]).
write_grid([NUM-ITEM|T]):-
    divmod(NUM,3,Q,R),
    (R == 2 -> writeln(ITEM) ; write(ITEM)),
    write_grid(T).

