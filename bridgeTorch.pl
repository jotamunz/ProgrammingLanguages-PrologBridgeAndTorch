:- dynamic(crossTime/2).
:- dynamic(maxTime/1).
:- dynamic(maxTorch/1).

% initialize, solve, and print the solution
start :- 
    insertPerson("Y"),
    insertTimeLimit,
    insertTorchLimit,
    initial(InitState),
    solve(InitState, [], Sol),
    forall(member(X, Sol),
    (write(X), nl)),
    reset.

% insert settings
insertPerson("Y") :-
    write("Inserte el nombre de una persona: "),
    read(Name),
    write("Inserte el tiempo que tarda en cruzar el puente: "),
    read(Time),
    assert(crossTime(Name, Time)),
    write("Desea ingresar otra persona? (Y/N): "),
    read(X),
    insertPerson(X).

insertPerson("N").

insertPerson(_) :-
    write("Comando no reconocido (Y/N): "),
    read(X),
    insertPerson(X).

insertTimeLimit :-
    write("Inserte el limite de tiempo para cruzar el puente: "),
    read(Time),
    assert(maxTime(Time)).

insertTorchLimit :-
    write("Inserte la cantidad de personas que puede iluminar la antorcha: "),
    read(Torch),
    assert(maxTorch(Torch)).

% remove all settings
reset :-
    retractall(crossTime(_,_)),
    retractall(maxTime(_)),
    retractall(maxTorch(_)).

% Start and end states
initial([0, l, Names, []]) :-
    findall(Name, crossTime(Name, _), Names).
    
final([_, r, [], _]).

% Recursively checks if a path can be made through all node combinations
solve(Node, Path, [Node|Path]) :- 
    final(Node).
solve(Node, Path, Sol) :- 
    move(Node, Movement),
    update(Node, Movement, NewNode),
    legal(NewNode),
    not(member(NewNode, Path)),
    solve(NewNode, [Node|Path], Sol).

% If the torch is on the left, calculate the max amount of crossers and generate all combs
% If the torch is on the right, generate all combinatios of 1 person
move([_, l, Left, _], Movement) :-
    crossers(Left, N),
    comb(N, Left, Movement).
move([_, r, _, Right], Movement) :-
    comb(1, Right, Movement).

% WIP
update([Time1, l, Left1, Right1], Movement, [Time2, r, Left2, Right2]) :-
    take(Movement, Left1, Left2),
    append(Movement, Right1, Right2),
    findTime(Movement, Time),
    Time2 is Time1 + Time.
update([Time1, r, Left1, Right1], Movement, [Time2, l, Left2, Right2]) :-
    take(Movement, Right1, Right2),
    append(Movement, Left1, Left2),
    findTime(Movement, Time),
    Time2 is Time1 + Time.

% WIP
legal([Time, _, _, _]) :-
    maxTime(X),
    Time < X.

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

% Generates all combinatios of N elements in a list
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

% retorna el tiempo maximo que le toma cruzar a 1 o 2 personas 
findTime([X], CrsTime) :- 
    crossTime(X, CrsTime).
findTime([X,Y], CrsTime) :- 
    crossTime(X, CrsTimeX),
    crossTime(Y, CrsTimeY),
    CrsTime is max(CrsTimeX, CrsTimeY).

% retorna el grupo de personas sin las personas especificadas 
take(People, Group, X) :- 
    findall(Z, (member(Z, Group),
    not(member(Z, People))), X).