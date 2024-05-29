%% Worksheet 23: Frequency Distribution
%%Here is another scattered partial map with state. Given a list of keys (here just integers), find the frequency %%distribution of the keys, and sort the keys into order. We shall represent 'count c of key k' as the term c*k. Example:
%%?- freq([3, 3, 2, 2, 1, 1, 2, 2, 3, 3], A). A = [2*1, 4*2, 4*3J
%5That is, there are two l's, four 2's, and four 3s. So this is something like run-length encoding, but each output entry %%gives the count (frequency) of each key in the whole input list.
%%We define the predicate freq such that the goal freq(L, S) succeeds for data list L and frequency list S:

freq(L, S) :- freq(L, [], S).
freq([], S, S).
freq([N I L], S1, S3) :- update(N, S1, S2), freq(L, S2, S3).
%%The output list is modified using the update predicate, which is respon- sible for inserting the keys into the correct %%order.
/* update(Key, BeforeList, AfterList) */

update(N, [], [1 *N)).
update(N,[F*NIS],[F1*NIS)) :- !,F1 isF+1. update(N,[F*MIS],[1*N,F*MIS)) :- N<M,!.
update(N, [F*M IS], [F*M I S1)) :- N \== M, update(N, S, S1).
Practice. Explain why the presence of a cut renders the '\==' goal unnecessary in the fourth clause