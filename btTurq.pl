
% Estados 
initial([0, l, [a,b,c,d], []]).
final([17, r, [], [a,b,c,d]]).

% Lados opuestos 
opp(l, r).  
opp(r, l).

% Tiempos para cruzar
crossTime(a, 1).
crossTime(b, 2).
crossTime(c, 5).
crossTime(d, 10).

% inicializa el estado e imprime la solucion
start :- 
    initial(InitState),
    path(InitState, [], Sol),
    forall(member(X, Sol),
    (write(X), nl)).

% Recursivamente revisa si puede hacer un camino probando todos los nodos
path(Node, Path, [Node|Path]) :- 
    final(Node).
path(Node, Path, Sol) :- 
    arc(Node, NewNode),
    not(member(NewNode, Path)),
    path(NewNode, [Node|Path], Sol).

% genera una movida y revisa si es valida 
arc([T1, F1, L1, R1], [T2, F2, L2, R2]) :- 
    opp(F1,F2),
	(
        (
            F1 = l,
            cross(L1, X), 
            take(X, L1, L2),
            append(X, R1, R2),
            findTime(X, T),
            T2 is T1 + T
        );
        (
            F1 = r,
            cross(R1, X), 
            take(X, R1, R2),
            append(X, L1, L2),
            findTime(X, T),
            T2 is T1 + T
        )
    ),
    T2 < 18.

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