dentroDelTablero(X,Y):-
    /* Mira Si El Valor De X Esta Entre 1 Y 8 */
    between(1,8,X),
    /* Mira Si El Valor De Y Esta Entre 1 Y 8 */
    between(1,8,Y).

jugadorCorrecto(X):-
    ((X == negras) , ! );
    (X == blancas).

verificarPieza(Z,X,Y):-
    jugadorCorrecto(Z),
    dentroDelTablero(X,Y).

peon(Jugador,Fila,Columna):-
    verificarPieza(Jugador,Fila,Columna).

torre(Jugador,Fila,Columna):-
    verificarPieza(Jugador,Fila,Columna).

caballo(Jugador,Fila,Columna):-
    verificarPieza(Jugador,Fila,Columna).

alfil(Jugador,Fila,Columna):-
    verificarPieza(Jugador,Fila,Columna).

dama(Jugador,Fila,Columna):-
    verificarPieza(Jugador,Fila,Columna).

rey(Jugador,Fila,Columna):-
    verificarPieza(Jugador,Fila,Columna).

esPeonBlanco(peon(blancas,_,_)).
esPeonNegro(peon(negras,_,_)).

esTorreBlanca(torre(blancas,_,_)).
esTorreNegra(torre(negras,_,_)).

esCaballoBlanco(caballo(blancas,_,_)).
esCaballoNegro(caballo(negras,_,_)).

esAlfilBlanco(alfil(blancas,_,_)).
esAlfilNegro(alfil(negras,_,_)).

esDamaBlanca(dama(blancas,_,_)).
esDamaNegra(dama(negras,_,_)).

esReyBlanco(rey(blancas,_,_)).
esReyNegro(rey(negras,_,_)).

:- use_module(library(lists)).

count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

coordenadas(X):-
    

eachValido([]).
eachValido([Head|Tail]):-
    Head,
    eachValido(Tail).

valido([]).
valido(X):-
    eachValido(X),
    
    between(-1,10,AB),
    count(X,alfil(blancas,_,_),AB),

    between(-1,10,AN),
    count(X,alfil(negras,_,_),AN),

    between(-1,10,TB),
    count(X,torre(blancas,_,_),TB),

    between(-1,10,TN),
    count(X,torre(negras,_,_),TN),

    between(-1,9,DB),
    count(X,dama(blancas,_,_),DB),

    between(-1,9,DN),
    count(X,dama(negras,_,_),DN),

    between(-1,1,RB),
    count(X,rey(blancas,_,_),RB),

    between(-1,1,RN),
    count(X,rey(negras,_,_),RN),

    between(-1,10,CB),
    count(X,caballo(blancas,_,_),CB),

    between(-1,10,CN),
    count(X,caballo(negras,_,_),CN),

    between(-1,8,PB),
    count(X,peon(blancas,_,_),PB),
    
    between(-1,8,PN),
    count(X,peon(negras,_,_),PN), ! .