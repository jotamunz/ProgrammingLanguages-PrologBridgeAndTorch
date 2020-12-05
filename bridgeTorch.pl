:- dynamic(crossTime/2).
:- dynamic(maxTime/1).
:- dynamic(maxTorch/1).

% Temporary Settings
maxTorch(2).
maxTime(28).
crossTime(a,1).
crossTime(b,2).
crossTime(c,5).
crossTime(d,10).
crossTime(e,15).
%crossTime(f,20).

% Insert settings
setup :- 
    insertPerson("Y"),
    insertTimeLimit,
    insertTorchLimit.

insertPerson("Y") :-
    write("Ingrese el nombre de una persona: "),
    read(Name),
    atom(Name),
    write("Ingrese el tiempo que tarda en cruzar el puente: "),
    read(Time),
    rational(Time),
    assert(crossTime(Name, Time)),
    write("Desea ingresar otra persona? (Y/N): "),
    read(X),
    insertPerson(X).

insertPerson("N").

insertTimeLimit :-
    write("Ingrese el limite de tiempo para cruzar el puente: "),
    read(Time),
    rational(Time),
    assert(maxTime(Time)).

insertTorchLimit :-
    write("Ingrese la cantidad de personas que puede iluminar la antorcha: "),
    read(Torch),
    integer(Torch),
    assert(maxTorch(Torch)).

% Solve and print
solveDepthFirst :-
    initial(InitState),
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    solveDF(InitState, [], Sol),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    forall(member(X, Sol), (write(X), nl)),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

solveHillClimb :-
    initial(InitState),
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    solveHC(InitState, [], Sol),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    forall(member(X, Sol), (write(X), nl)),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

% Remove all settings
reset :-
    retractall(crossTime(_,_)),
    retractall(maxTime(_)),
    retractall(maxTorch(_)).

% Start and end states
initial([0, l, Names, []]) :-
    findall(Name, crossTime(Name, _), Names).
    
final([_, r, [], _]).

/*
*  DEPTH FIRST SEARCH
*/

% Recursively checks if a path can be made through all node combinations
solveDF(Node, Path, [Node|Path]) :- 
    final(Node).
solveDF(Node, Path, Sol) :- 
    move(Node, Movement),
    update(Node, Movement, NewNode),
    legal(NewNode),
    not(member(NewNode, Path)),
    solveDF(NewNode, [Node|Path], Sol).

% Calculate the max amount of crossers permitted and generate all combs for that amount
move([_, l, Left, _], Movement) :-
    crossers(Left, N),

    % This method generates all combs for the max amount permitted to the least 
%    inverseBetween(1, N, X),
%    comb(X, Left, Movement).

    % This method cuts the posibility tree by never crossing less than the max amount permitted
    comb(N, Left, Movement).
move([_, r, _, Right], Movement) :-
    
    % This method generates all combs for the least amount permitted to the max 
%    crossers(Right, N),
%    between(1, N, X),
%    comb(X, Right, Movement).

    % This method cuts the posibility tree by never crossing more than 1
    comb(1, Right, Movement).

% Moves people from one side to another and updates the total time based on the slowest
update([Time1, l, Left1, Right1], Movement, [Time2, r, Left2, Right2]) :-
    take(Movement, Left1, Left2),
    append(Movement, Right1, Right2),
    findTimes(Movement, Times),
    maxList(Times, MaxTime),
    Time2 is Time1 + MaxTime.
update([Time1, r, Left1, Right1], Movement, [Time2, l, Left2, Right2]) :-
    take(Movement, Right1, Right2),
    append(Movement, Left1, Left2),
    findTimes(Movement, Times),
    maxList(Times, MaxTime),
    Time2 is Time1 + MaxTime.

% Checks if the total time is less than the max time
legal([Time, _, _, _]) :-
    maxTime(X),
    Time =< X.

% If there are more people than the max capacity, cross the max
% If there are less people than the max capacity, cross them all
crossers(Group, X) :-
    maxTorch(N),
    length(Group, Len),
    Len >= N,
    X is N.
crossers(Group, X) :-
    maxTorch(N),
    length(Group, Len),
    Len < N,
    X is Len.

% Generates an array with the times of a group of people
findTimes([], []).
findTimes([Name|People], [Time|CrsTimes]) :- 
    crossTime(Name, Time),
    findTimes(People, CrsTimes).

% Generates all combinations of N elements in a list
comb(N, List, X) :-
    length(X, N),
    mem1(X, List).

mem1([], Y).
mem1([H|T], Y) :- 
    member(H, Y),
    rest(H, Y, New),
    mem1(T, New).

rest(A, List, R) :- 
    append(_, [A|R], List), !.

% Removes the given elements from a list
take(Elem, List, X) :- 
    findall(Z, (member(Z, List), not(member(Z, Elem))), X).

% Obtains the max number from a list
maxList(List, M):- 
    member(M, List), 
    findall(X, (member(X, List), X > M), New),
    length(New, 0).

% Obtains all numbers in an inclusive range ordered from largest to smallest
inverseBetween(L, H, H) :- 
    H >= L.
inverseBetween(L, H, X) :- 
    H1 is H - 1, 
    H1 >= L, 
    inverseBetween(L, H1, X).

/*
*  HILL CLIMBING
*/

% Recursively checks if a path can be made through all node combinations in order according to value
solveHC(Node, Path, [Node|Path]) :- 
    final(Node).
solveHC(Node, Path, Sol) :- 
    hillClimb(Node, Movement),
    update(Node, Movement, NewNode),
    legal(NewNode),
    not(member(NewNode, Path)),
    solveHC(NewNode, [Node|Path], Sol).

% Generates all node combinations from a node and orders them according to value
hillClimb(Node, Movement) :-
    findall(X, move(Node, X), Moves),
    evaluateOrder(Node, Moves, [], OrderedMoves),
    member((Movement, _), OrderedMoves).

% Recursively evaluates all moves and orders the list by highest value
evaluateOrder(_, [], Accumulated, Accumulated).
evaluateOrder(Node, [Movement|Moves], Accumulated, OrderedMoves) :-
    update(Node, Movement, NewNode),        
    value(NewNode, Value),               
    insertPair((Movement, Value), Accumulated, Z), 
    evaluateOrder(Node, Moves, Z, OrderedMoves).

% Inserts a tuple in an ordered list based on the second element
insertPair(MV, [], [MV]).
insertPair((M, V), [(M1, V1)|MVs], [(M, V), (M1, V1)|MVs]) :-
    V >= V1.
insertPair((M, V), [(M1, V1)|MVs], [(M1, V1)|MVs1]) :-
    V < V1,
    insertPair((M, V), MVs, MVs1).

value([Time, l, _, _], Value) :-
    maxTime(T),
    Value is T - Time.

value([Time, r, Left, Right], 0).