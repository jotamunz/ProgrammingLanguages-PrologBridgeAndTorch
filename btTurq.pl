:- dynamic(crossTime/2).
:- dynamic(maxTime/1).

% insert run conditions, solve, and print the solution
start :- 
    insertPerson("Y"),
    insertTimeLimit,
    initial(InitState),
    solve(InitState, [], Sol),
    forall(member(X, Sol),
    (write(X), nl)).

% insert a person with their crossing time
insertPerson("Y") :-
    write("Inserte el nombre de una persona "),
    read(Name),
    write("Inserte el tiempo que tarda en cruzar el puente "),
    read(Time),
    assert(crossTime(Name, Time)),
    write("Desea ingresar otra persona? (Y/N) "),
    read(X),
    insertPerson(X).

insertPerson("N").

insertPerson(_) :-
    write("Comando no reconocido (Y/N) "),
    read(X),
    insertPerson(X).

% insert time limit to cross the bridge
insertTimeLimit :-
    write("Inserte el limite de tiempo para cruzar el puente "),
    read(Time),
    assert(maxTime(Time)).

% Start and end states
initial([0, l, Names, []]) :-
    findall(Name, crossTime(Name, _), Names).
    
final([_, r, [], _]).

% Recursivamente revisa si puede hacer un camino probando todos los nodos
solve(Node, Path, [Node|Path]) :- 
    final(Node).
solve(Node, Path, Sol) :- 
    move(Node, Movement),
    update(Node, Movement, NewNode),
    legal(NewNode),
    not(member(NewNode, Path)),
    solve(NewNode, [Node|Path], Sol).

% WIP
move([_, l, Left, _], Movement) :-
    cross(Left, Movement).
move([_, r, _, Right], Movement) :-
    cross(Right, Movement).

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

% retorna todas las combinaciones de 1 persona y 2 personas del grupo [a,b,c,d] 
cross(Group, X) :- 
    comb(1, Group, X); 
    comb(2, Group, X).

comb(N, Group, X) :-
    length(X, N),
    mem1(X, Group).

mem1([], Y).
mem1([H|T], Y) :- 
    member(H, Y),
    rest(H, Y, New),
    mem1(T, New).

rest(A, Group, R) :- 
    append(_, [A|R], Group), !.

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