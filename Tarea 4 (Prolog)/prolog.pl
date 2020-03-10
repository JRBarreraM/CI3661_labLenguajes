* :- 0.
up(*) :- 1.

up(X) :- X+1.

suma(up(up(*)), up(*), X) :-
    X is up(up(up(*))).

run :-
    suma(up(up(*)),up(*),X).