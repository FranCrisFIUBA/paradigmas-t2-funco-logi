%% Base de conocimientos de ejemplo: recorridos recursivos y strings
%% Cubre recursión con decremento, eqStr, len y built-ins anidados

bondi(retiro,palermo).
bondi(palermo,belgrano).
bondi(belgrano,nunez).

puedeLlegar(X,Y,N):-bondi(X,Y).
puedeLlegar(X,Y,N):-gt(N,0),N1=sub(N,1),bondi(X,Z),puedeLlegar(Z,Y,N1).

nombreValido(S):-not(eqStr(S,"")),gt(len(S),3).
esIgual(A,B):-eqStr(A,B).
