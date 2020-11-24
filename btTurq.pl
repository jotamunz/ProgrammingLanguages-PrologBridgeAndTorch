
/* Estados */
initial([0, l, [a,b,c,d], []]).
final([17, r, [], [a,b,c,d]]).

/* Lados opuestos */
opp(l, r).  
opp(r, l).

/* Tiempos para cruzar */
crossTime(a, 1).
crossTime(b, 2).
crossTime(c, 5).
crossTime(d, 10).

/* retorna todas las combinaciones de 1 persona y 2 personas del grupo [a,b,c,d] */
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

/* retorna el tiempo maximo que le toma cruzar a 1 o 2 personas */
findTime([X], CrsTime) :- 
    crossTime(X, CrsTime).
findTime([X,Y], CrsTime) :- 
    crossTime(X, CrsTimeX),
    crossTime(Y, CrsTimeY),
    CrsTime is max(CrsTimeX, CrsTimeY).

/* retorna el grupo de personas sin las personas especificadas */
take(People, Group, X) :- 
    findall(Z, (member(Z, Group),
    not(member(Z, People))), X).