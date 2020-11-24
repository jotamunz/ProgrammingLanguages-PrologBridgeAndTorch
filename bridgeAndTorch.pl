
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

