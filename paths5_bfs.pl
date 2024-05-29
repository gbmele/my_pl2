%We may now implement a breadth-first search for state spaces. Actually we would not like to tie our breadth-first %implementation to any particular successor predicate. Instead, we will write it as a higher-order predicate: it will take %a parameter Succ, which can be an arbitrary predicate. That way we can write bfs just once and reuse it to search any %space we like.

%Here is the implementation:



succ(a,b).
succ(b,c).
succ(b,e).
succ(c,d).
succ(b,d).
succ(d,e).


% helper predicate
prepend(L, X, [X | L]).

% if goal is at the head of the queue, return it
bfs(_, [[Goal | Rest] | _], _, Goal, [Goal | Rest]).

% main recursive predicate: bfs(+Succ, +Queue, +Visited, +Goal, -Solution)
bfs(Succ, [[State | Path] | Queue], Visited, Goal, Solution) :-
    findall(X, call(Succ, State, X), Next),    % find all neighboring states
    subtract(Next, Visited, Next1),            % remove already-visited states
    maplist(prepend([State | Path]), Next1, Next2), % prepend each state to path
    append(Queue, Next2, Queue2),              % add all new states to queue
    append(Next1, Visited, Visited1),          % add all new states to visited set
    bfs(Succ, Queue2, Visited1, Goal, Solution).   % recurse

% top-level predicate: bfs(+Succ, Start, +Goal, -Solution)
bfs(Succ, Start, Goal, Solution) :-
    bfs(Succ, [[Start]], [Start], Goal, Solution1),

    reverse(Solution1, Solution).