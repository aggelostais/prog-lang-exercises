:-dynamic(edge/2).

coronograph(File, Answers) :-
    once(read_file(File, Answers)). %me to once tha paraksei mia apodeixi

is_connected(Root, Size_of_graph) :-
    dfs_connected(Root, After_dfs_visited),
    length(After_dfs_visited, Length),
    Length =:= Size_of_graph.

dfs_connected(Root, After_dfs_visited) :-
    dfs_connected([Root], [], After_dfs_visited).

dfs_connected([],Visited, Visited).
dfs_connected([H|T],Visited, After_dfs_visited) :-
    member(H,Visited),
    dfs_connected(T,Visited, After_dfs_visited).

dfs_connected([H|T],Visited, After_dfs_visited) :-
    not(member(H,Visited)),
    findall(Nb,edge(H,Nb),Nbs),
    append(Nbs,T, ToVisit),
    dfs_connected(ToVisit,[H|Visited], After_dfs_visited).

% code for reading

read_file(File, Answers) :-
    open(File, read, Stream),
    % writeln("file opened"),
    read_line(Stream, [Size]),
    % write("size = "),
    % writeln(Size),
    while(File, Stream, Size, [], Answers).

while(_,_,I, CurrAns, CurrAns) :-
    I < 1.
while(File, Stream, Size, CurrAns, Answers) :-
    Size > 0,
    % write("on while "), writeln(Size),
    read_input(File, Stream, N, _),
    corona(N, Answer),
    % writeln(Answer),
    (["'NO CORONA'"] = Answer ->
                                [H] = Answer,
                                append(CurrAns, [H], NextAnswers)
    ;
        append(CurrAns, [Answer], NextAnswers)
    ),
    !,
    retractall(edge(_,_)),
    NextSize is Size - 1,
    while(File, Stream, NextSize, NextAnswers, Answers).

read_input(_File, Stream, N, M) :-
    % open(File, read, Stream),
    read_line(Stream, [N, M]),
    read_lines(Stream, M),
    !.

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

read_lines(_, 0).
read_lines(Stream, Iteration) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L1),
    [First, Second] = L1,
    assert(edge(First, Second)),
    assert(edge(Second, First)),
    NextIteration is Iteration - 1,
    read_lines(Stream, NextIteration).

% end of reading code


extract_cycle_h(List, Target,1, Acc, Return) :-
    [H|T] = List,
    (H = Target -> append(Acc, [H], Return)
    ;
        append(Acc, [H], NextAcc),
        extract_cycle_h(T, Target,1, NextAcc, Return)
    ).
extract_cycle_h(List, Target,0, Return) :-
    [H|T] = List,
    extract_cycle_h(T, Target, 1, [H], Return).

extract_cycle(Ext, Res) :-
    reverse(Ext, RevExt),
    [Target|_] = RevExt,
    extract_cycle_h(RevExt, Target, 0, Res).


cycle(Node,Cycle) :-
    cycle(Node,-1,[],Cycle).

cycle(Curr, _,Visited,Cycle) :-
    member(Curr,Visited),
    !,
    reverse([Curr|Visited],Cycle).

cycle(Curr, Father,Visited,Cycle) :-
    edge(Curr,Next),
    not(Next = Father),
    cycle(Next, Curr,[Curr|Visited],Cycle).


corona(N, Ans) :-
    findall(Cs, cycle(1, Cs), Cycles),
    length(Cycles, Count),
    ( (Count == 2 , is_connected(1, N) ) ->
                    % write("Corona "),
                    [Extended_cycle|_] = Cycles,
                    extract_cycle(Extended_cycle, Main_cycle),
                    [_|Cycle_list] = Main_cycle,
                    length(Cycle_list, Number_of_Roots),
                    % writeln(Number_of_Roots),
                    append([], [Number_of_Roots], AnswerList),
                    trees(Cycle_list, Cycle_list, [], TreesList),
                    % write("TreesList = "),
                    % writeln(TreesList),
                    msort(TreesList, Sorted),
                    % write("Sorted TreesList = "),
                    % writeln(Sorted)
                    append(AnswerList, [Sorted], Ans)
    ;
        % writeln("No Corona")
        append([], ["'NO CORONA'"], Ans)
    ),
    !.

trees([], _, Accum, Accum).
trees(Remaining, Roots, Accum, Res) :-
    [Node | Rest] = Remaining,
    dfs(Node, Roots, Ret), %Roots binds with Visited
    append(Accum, [Ret], NextAccum),
    trees(Rest, Roots, NextAccum, Res).

dfs(Node, Visited, Result):-
    findall(Neighbor, edge(Node, Neighbor), Neighbors),
    (not(member(Node, Visited)) -> append(Visited, [Node], NewVisited)
    ;
        NewVisited = Visited
    ),
    ([One] = Neighbors , member(One, NewVisited) -> Result is 1
    ;
        dfs_list(Neighbors, NewVisited, Chld_Res),
        Result is Chld_Res + 1
    ).

dfs_list([], _, 0).
dfs_list([H | Rest], Visited, Res) :-
    (member(H, Visited) -> dfs_list(Rest, Visited, Res)
    ;
        dfs(H, Visited, Result1),
        dfs_list(Rest, [H | Visited], Result2),
        Res is Result1 + Result2
    ).
