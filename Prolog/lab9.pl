%max(+X,+Y,-Z).
max(X,Y,X):- X>Y, !.
max(_,Y,Y).

%max(+X,+Y,+Z,-U)
max(X,Y,Z,U):- max(X,Y,V), max(V,Z,U).

max3(X,Y,Z,X):- X>Y, X>Z, !.
max3(_,Y,Z,Y):- Y>Z, !.
max3(_,_,Z,Z).

%Factorial
fact_up(0, 1).
fact_up(N, X) :-
    N > 0,
    N1 is N - 1,
    fact_up(N1, X1),
    X is N * X1.

fact_down(N, X) :-
    fact_tail(N, 1, X).

fact_tail(0, Acc, Acc).
fact_tail(N, Acc, X) :-
    N > 0,
    NewAcc is N * Acc,
    N1 is N - 1,
    fact_tail(N1, NewAcc, X).

%sumCifr(+N,?sum)
sumCifrUp(0,0):- !.
sumCifrUp(N,Sum):-
    Cifr is N mod 10,
    N1 is N div 10,
    sumCifrUp(N1,Sum1),
    Sum is Sum1 + Cifr.

sumCifrDown(0,CurSum,CurSum):- !.
sumCifrDown(N,CurSum,Sum):-
    Cifr is N mod 10,
    N1 is N div 10,
    CurSum is CurSum+Cifr,
    sumCifrDown(N1,CurSum,Sum).
sumCifrDown(N,Sum):- sumCifrDown(N,0,Sum).


r_list(A,N):-r_list(A,N,0,[]).
r_list(A,N,N,A):-!.
r_list(A,N,K,B):-read(X),append(B,[X],B1),K1 is K+1,r_list(A,N,K1,B1).


%Write_to_list
w_list([]):-!.
w_list([H|T]):-write(H),nl,w_list(T).

%Append_to_list
my_append([], X, Y).
my_append([X|Tail], Y, [X|Tail1]):- my_append(Tail, Y, Tail1).

%Check_in_list
in_list([], _):- false.
in_list([X|Tail], X):- !.
in_list([_|Tail], X):- in_list(Tail, X).

%Get_by_index
get_at(0, [Head|_], Head).

get_at(Ix, [_|Tail], Element) :-
    Ix > 0,
    NewIx is Ix - 1,
    get_at(NewIx, Tail, Element).

%Sum_of_elements_list
sum_list_up([], 0).

sum_list_up([H|T], Sum) :-
    sum_list_up(T, TS),
    Sum is H + TS.

sum_list_down(List, Sum) :-
    sum_list_down(List, 0, Sum).

sum_list_down([], Acc, Acc).

sum_list_down([H|T], Acc, Sum) :-
    NewAcc is Acc + H,
    sum_list_down(T, NewAcc, Sum).

% Удаление элементов с неравной суммой цифр
remove_if_sum_equal([], _, []).

remove_if_sum_equal([X | Tail], TargetSum, Result) :-
    digit_sum_down(X, Sum),
    (Sum =:= TargetSum -> remove_if_sum_equal(Tail, TargetSum, Result); Result = [X | NewResult], remove_if_sum_equal(Tail, TargetSum, NewResult)).




