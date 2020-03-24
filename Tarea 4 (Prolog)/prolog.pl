church(*).

church(up(A)) :- church(A).

suma(A, *, A) :- church(A).
suma(A, up(B), up(C)) :-
    suma(A, B, C).

resta(A, *, A) :- church(A).
resta(up(A), up(B), C) :-
    resta(A, B, C).

producto(A, *, *) :- church(A).
producto(A, up(B), C) :-
    suma(D, A, C),
    producto(A, B, D).