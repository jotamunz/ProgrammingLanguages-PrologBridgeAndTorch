/**
* Lenguajes de Programacion - Tarea Programada 2
* Andres Esteban Aguilar Moya - 2019156214
*/

/**
 * Estado inicial:
 * Lugar de la antorcha, tiempo actual, tiempo limite,
 * personas a la izquierda, personas a la derecha
 */
initial_state(bridges_torch, bridges_torch(izq, 2, 0, 28, [
      persona(a, 1),
      persona(b, 2),
      persona(c, 5),
      persona(d, 10),
      persona(e, 15)
], [])).


% Estado final del programa
% Las personas se encuentran a la derecha
% y no hay nadie a la izquierda
final_state(bridges_torch(der, _, _, _, [], _)).

% Si se llega al estado final, las personas
% no tienen que moverse
solve_bridges_depth_first(Estado, _, []) :- final_state(Estado).

% Si no se llega al estado final sucede lo siguiente
solve_bridges_depth_first(Estado,Historia,[Movida|Movidas]) :-
      move(Estado, Movida),          % Dado un estado se genera una movida
      update(Estado,Movida,Estado2), % Actualizar el estado con la nueva movida
      legal(Estado2),
      not(member(Estado2,Historia)),
      solve_bridges_depth_first(Estado2,[Estado2|Historia],Movidas).
/*    
 * Inicializa un problema y lo resuelve.
 *   Problema: nombre del problema.
 *   Movidas: movidas requeridas para resolver el problema.
 */
test_solution(Problema,Movidas) :-
      initial_state(Problema,Estado),      % obtener un Estado inicial dado Problema
      solve_bridges_depth_first(Estado,[Estado],Movidas).  % inicia resolucion desde estado

% Si la antorcha se encuentra a la derecha, tomar cualquier persona de la derecha
move(bridges_torch(der, _, _, _, _, PersonasDerecha), PersonaQueViajara):-
      create_combination(1, PersonasDerecha, PersonaQueViajara).
move(bridges_torch(izq, N, _, _, PersonasIzquierda, PersonasDerecha), PersonasQueViajan):-
      create_combination(N, PersonasIzquierda, PersonasQueViajan).

% Gets all the possible combinations for List with
% a size of Size
create_combination(Size, List, Combination):-
      length(Combination, Size),
      mem1(Combination, List).

/* mem1(Lr,L). For comb/3. Same as mem/2 but does not generate [a,b] and [b,a]. 	
	?- mem1([X,Y],[a,b,c]).
	[a,b][a,c][b,c]
*/
mem1([],Y).
mem1([H|T],Y):-member(H,Y),rest(H,Y,New),mem1(T,New).

rest(A,L,R):- append(_,[A|R],L),!.


update(bridges_torch(AntorchaVieja, N, TiempoActual, TiempoLimite, PersonasIzquierda1, PersonasDerecha1),
PersonasViajando, 
bridges_torch(AntorchaNueva, N, TiempoNuevo, TiempoLimite, PersonasIzquierda2, PersonasDerecha2)):-
      update_antorcha(AntorchaVieja, AntorchaNueva), % Actualizar lugar de la antorcha
      update_extremos(AntorchaVieja, PersonasViajando, PersonasIzquierda1, PersonasDerecha1, PersonasIzquierda2, PersonasDerecha2), % TODO: Actualizar extremos
      update_tiempo(TiempoActual, PersonasViajando, TiempoNuevo). % TODO: Actualizar tiempos

% Cambiar de lado la antorcha
update_antorcha(izq, der).
update_antorcha(der, izq).

update_extremos(izq, PersonasViajando, PersonasIzquierda1, PersonasDerecha1, PersonasIzquierda2, PersonasDerecha2):-
      take(PersonasViajando, PersonasIzquierda1, PersonasIzquierda2),
      append(PersonasViajando, PersonasDerecha1, PersonasDerecha2).

update_extremos(der, PersonasViajando, PersonasIzquierda1, PersonasDerecha1, PersonasIzquierda2, PersonasDerecha2):-
      take(PersonasViajando, PersonasDerecha1, PersonasDerecha2),
      append(PersonasViajando, PersonasIzquierda1, PersonasIzquierda2).

/* remove all elements in S from L result is in R */
take(S,L,R):- findall(Z,(member(Z,L),not(member(Z,S))),R).

update_tiempo(TiempoActual, PersonasViajando, TiempoNuevo):-
      create_list_of_times(PersonasViajando, ListaNum),
      max(ListaNum, MaxT),
      TiempoNuevo is MaxT + TiempoActual.

create_list_of_times([], []).
create_list_of_times([persona(Nombre, Tiempo)|Tail], [Tiempo|T]):-
      create_list_of_times(Tail, T).

max(L, M):- 
      member(M,L), 
      findall(X, (member(X, L), X > M), NL),
      length(NL, 0).

legal(bridges_torch(_, _, TiempoActual, TiempoLimite, _, _)):-
      not(ilegal(TiempoActual, TiempoLimite)).

% Reglas para ilegalidad
ilegal(TiempoActual, TiempoLimite):- TiempoActual > TiempoLimite.




