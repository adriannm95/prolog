% Del Pos
delP([],_,[]):-!.
delP(X,P,X):- P = 0,!.
delP([X|B],P,[X|R]):- P > 1, P1 is P - 1, delP(B,P1,R).
delP([_|B],P,R):- P = 1, P1 is P - 1, delP(B,P1,R).


% Delete N-N
drop(L1,N,L2) :- drop(L1,N,L2,N).
drop(X,0,X,0).
drop([],_,[],_).
drop([_|Xs],N,Ys,1) :- drop(Xs,N,Ys,N).
drop([X|Xs],N,[X|Ys],K) :- K > 1, K1 is K - 1, drop(Xs,N,Ys,K1).

% Difference A B
dif([],[],[]):-!.
dif([],_,[]):-!.
dif(X,[],X):-!.
dif([X|XB],[Y|YB],R):- containsD(X,[Y|YB]), dif(XB,[Y|YB],R).
dif([X|XB],[Y|YB],[X|R]):-dif(XB,[Y|YB],R).

containsD(_,[]):-false.
containsD(E,[X|_]):-E = X,true.
containsD(E,[X|B]):- E \= X,containsD(E,B).



% Return the deep Element
deepE([],_,0):- !.
deepE([C1|_],N,R2):- list(C1), deepE(C1,N,R),R \= 0,
  R2 is R + 1,!.

deepE([C1|_],N,R):-list(C1), deepE(C1,N,R), R = 0,!.

deepE([C1|_],N,1):-not(list(C1)), C1 = N,!.

deepE([C1|B],N,R):-not(list(C1)), C1 \= N, deepE(B,N,R).

list([]):-!.
list([_|B]):- list(B).


% Intersection A B
intersecc([],[],[]):-!.
intersecc([],_,[]):-!.
intersecc(_,[],[]):-!.
intersecc([X|XB],[Y|YB],[X|R]):-contenIsec(X,[Y|YB]), intersecc(XB,[Y|YB],R),!.
intersec([_|XB],[_|YB],R):- intersec(XB,YB,R).

contenIsec(_,[]):-false.
contenIsec(A,[Y|_]):- A = Y,true.
contenIsec(A,[Y|B]):- A \= Y, contenIsec(A,B).

% mix A B
zipp([],[],[]):-!.
zipp(A,[],A):-!.
zipp([],B,B):-!.
zipp([X|XB],[Y|YB],[X,Y|R]):- zipp(XB,YB,R).


lengthAux([],0):-!.
lengthAux([_|B],1):-B = [],!.
lengthAux([_|B],R):- lengthAux(B,R1),R is R1 + 1.

% Union A B
union([],X,X):-!.
union([X|B],Y,R) :-unionAux(X,Y),union(B,Y,R).
union([X|B],Y,[X|R]) :- union(B,Y,R).

unionAux(_,[]):-false,!.
unionAux(E,[X|_]):- E = X,true,!.
unionAux(E,[X|B]):- E \= X,unionAux(E,B).

% N last element
nlast([],[]):-!.
nlast([X,_|B],X):- B = [],!.
nlast([_|B],R):- nlast(B,R).


% Even list pos
evenPos([],[]):-!.
evenPos([_],[]):-!.
evenPos([_,X2|B],[X2|B1]):-evenPos(B,B1).

% Odd list pos
oddPos([],[]):-!.
oddPos([X],[X]):-!.
oddPos([X1,_|B],[X1|B1]):-oddPos(B,B1).

% Flatten a list
flatten([],[]):-!.
flatten([C1|B],R):-is_list(C1),flatten(C1,R1),flatten(B,R2),append(R1,R2,R),!.
flatten([C1|B],[C1|R1]):-not(is_list(C1)),flatten(B,R1).

contains([],[]):-true,!.
contains([],_):- true,!.
contains([A|AC],[B|BC]):-(aux([A|AC],[B|BC]) -> true ; contains([A|AC],BC)).

aux([],_):- true,!.
aux([C1|B],[D1|G]):- C1 = D1 -> aux(B,G) ; false.


eliUN([],_,[]):-!.
eliUN([_],N,[]):- N >= 1,!.
eliUN(X,N,X):- N < 1,!.
eliUN([_|B],N,[]):-lengthAux(B,L), T is N - 1,L=< T,!.
eliUN([X|B],N,[X|R]):-eliUN(B,N,R).



palindrome(L) :- invert(L,L).

invert([],[]):-!.
invert([X],[X]):-!.
invert([P1|C],L):-invert(C,X),append(X,[P1],L).
