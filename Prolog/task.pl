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
