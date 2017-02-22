% Crack The Code - Solution to silly game circulating on facebook (https://i.stack.imgur.com/ZHs7v.jpg)

secret(X,Y,Z) :-
    cond1(X,Y,Z),
    cond2(X,Y,Z),
    cond3(X,Y,Z),
    cond4(X,Y,Z),
    cond5(X,Y,Z).


% condicao 1:
% 682
% um numero correto e no lugar certo
cond1(6,_,_).
cond1(_,8,_).
cond1(_,_,2).

% condicao 2:
% 614
% um numero correto, mas no lugar errado
cond2(_,6,_).
cond2(_,_,6).
cond2(1,_,_).
cond2(_,_,1).
cond2(4,_,_).
cond2(_,4,_).


% condicao 3:
% 206
% dois numeros corretos, mas no lugar errado
cond3(_,2,0).
cond3(0,_,2).
cond3(0,2,_).
cond3(_,6,2).
cond3(6,_,2).
cond3(6,2,_).
cond3(_,6,0).
cond3(6,_,0).
cond3(0,6,_).

% condicao 4:
% 738
% nada esta correto
cond4(X,Y,Z) :-
    X \= 7, X \= 3, X \= 8,
    Y \= 7, Y \= 3, Y \= 8,
    Z \= 7, Z \= 3, Z \= 8.


% condicao 5:
% 780
% Um numero correto, mas no lugar errado
cond5(_,7,_).
cond5(_,_,7).
cond5(8,_,_).
cond5(_,_,8).
cond5(0,_,_).
cond5(_,0,_).

















