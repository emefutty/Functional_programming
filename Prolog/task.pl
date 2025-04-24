men:-man(X), write(X), nl, fail.

women:-woman(X), write(X), nl, fail.

children(X,Y):- parent(X,Y).

children(X):- parent(X,Y), write(Y), nl, fail.

mother(X,Y):- woman(X), parent(X,Y).
mother(Y):- parent(X,Y), woman(X), write(X), nl.

brother(X,Y):- man(X), parent(Z,X), parent(Z,Y), X\=Y.
brothers(X):- brother(Y,X), write(Y), nl, fail.

b_s(X,Y):- parent(Z,X), parent(Z,Y), X\=Y.
b_s(X):- b_s(X,Y), write(Y), nl, fail.


%Задание 2

father(X,Y) :- man(X), parent(X, Y).
father(X) :- father(Y, X), write(Y), nl, fail.

wife(X,Y) :- parent(X,Z), parent(Y,Z), woman(X), man(Y).
wife(X) :- wife(Y, X), write(Y), nl, fail.

%Задание 3

grand_ma(X, Y) :- woman(X), parent(X, Z), parent(Z, Y).
grand_mas(X):- grand_ma(Y,X), write(Y), nl, fail.

grand_pa_and_da(X,Y):- grand_ma(Z,X), wife(Z,Y); grand_ma(Z,Y), wife(Z,X).

niece(X, Y):- woman(X), parent(Z, X), b_s(Z, Y).
nieces(X):- niece(Y, X), write(Y), nl, fail.
