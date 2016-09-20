edificio(ed(3,6,5)).
edificio(ed(4,9,3)).
edificio(ed(8,11,2)).
resuelveSkyline([],[]):-!.
resuelveSkyline([ed(X1,X2,H)|[]],S) :- edificioAskyline(ed(X1,X2,H),S),!.
resuelveSkyline(E,S):- divide(E,L1,L2),resuelveSkyline(L1,S1),
	resuelveSkyline(L2,S2),combina(S1,S2,S).
edificioAskyline(ed(X1,X2,H),[c(X1,H),c(X2,0)]).
divide([],[],[]):- !.
divide([H|[]],[H|T1],[]) :- divide([],T1,[]).
divide([H1,H2|T],[H1|T1],[H2|T2]) :- divide(T,T1,T2).
combina(S1,S2,S) :- combinaAux(S1,S2,S,0,0,0).
combinaAux([],[],[],_,_,_):- !.
combinaAux([],S2,S2,_,_,_):- !.
combinaAux(S1,[],S1,_,_,_):- !.

combinaAux([c(X1,Y1)|T1],[c(X2,Y2)|T2],S,_,_,M):-
	X1 == X2,max(Y1,Y2,X),M==X -> combinaAux(T1,T2,S,Y1,Y2,M).
combinaAux([c(X1,Y1)|T1],[c(X2,Y2)|T2],[c(X1,X)|S],_,_,M):-
	X1 == X2,max(Y1,Y2,X),M\=X -> combinaAux(T1,T2,S,Y1,Y2,X).
combinaAux([c(X1,Y1)|T1],[c(X2,Y2)|T2],S,_,UY,M):-
	X1 < X2,max(Y1,UY,X),M==X -> combinaAux(T1,[c(X2,Y2)|T2],S,Y1,UY,M).
combinaAux([c(X1,Y1)|T1],[c(X2,Y2)|T2],[c(X1,X)|S],_,UY,M):-
	X1 < X2,max(Y1,UY,X),M\=X ->
	combinaAux(T1,[c(X2,Y2)|T2],S,Y1,UY,X).
combinaAux([c(X1,Y1)|T1],[c(X2,Y2)|T2],S,UX,_,M):-
	X1 > X2,max(Y2,UX,X),M==X -> combinaAux([c(X1,Y1)|T1],T2,S,UX,Y2,M).
combinaAux([c(X1,Y1)|T1],[c(X2,Y2)|T2],[c(X2,X)|S],UX,_,M):-
	X1 > X2,max(Y2,UX,X),M\=X ->
	combinaAux([c(X1,Y1)|T1],T2,S,UX,Y2,X).

max(X,Y,Z) :- X>=Y, Z is X,!.
max(X,Y,Z) :- X<Y, Z is Y,!.

% ----Parte opcional
listaAltura(L,R) :- listaAltura2(L,R,1,0).
listaAltura2([],[],_,_):-!.
listaAltura2([c(X,Y)|T],[Y|TS],I,_) :-
	I==X , I2 is I+1 ->listaAltura2(T,TS,I2,Y).
listaAltura2([c(X,Y)|T],[XH|TS],I,XH) :-
	I\=X , I2 is I+1 ->listaAltura2([c(X,Y)|T],TS,I2,XH).

dibujaSkyline(SK):- listaAltura(SK,L),max_member(M,L),dibujaSkyline2(L,M,L).

dibujaSkyline2([],IY,A):-
	IY\=0,I is IY-1 ->write('\n'),dibujaSkyline2(A,I,A).
dibujaSkyline2([],IY,_):-
	IY==0 ->write(' '),!.
dibujaSkyline2([_|T],IY,A):-
	IY==0 -> write(-),dibujaSkyline2(T,IY,A).
dibujaSkyline2([H|T],IY,A):-
	H>=IY,IY\=0 -> write(*), dibujaSkyline2(T,IY,A).
dibujaSkyline2([H|T],IY,A):-
	H<IY,IY\=0 -> write(' '), dibujaSkyline2(T,IY,A).
