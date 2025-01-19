% ---------------------------------------------------------
% PARTE 1 - Entrada do problema.
% container(Width, Height, Depth)
% caixa(Id, Width, Height, Depth).
% ---------------------------------------------------------

container(10, 10, 10).

caixa(1, 2, 3, 5).
caixa(2, 1, 2, 4).
caixa(3, 3, 4, 5).

% ---------------------------------------------------------
% PARTE 2 - Cálculo do custo de uma caixa e do total.
% O custo é o volume que a caixa ocupa.
% O custo total é a soma dos volumes das caixas alocadas.
% ---------------------------------------------------------

volume(W, H, D, Volume) :-
    Volume is W * H * D.

custo([], 0).
custo([alocada(_, _, _, _, W, H, D) | Outras], CustoTotal) :-
    volume(W, H, D, Volume),
    custo(Outras, Subtotal),
    CustoTotal is Subtotal + Volume.

custo_total(Alocadas, Custo) :-
    custo(Alocadas, Custo).

% ---------------------------------------------------------
% PARTE 3 - Valida Alocação
% alocada(Id, X, Y, Z, W, H, D) onde X, Y, Z sao as coordenadas e W, H, D sao as dimenções da caixa Id.
% Verifica se a caixa cabe no container ou se ela se sobrepõe a alguma caixa já alocada.
% ---------------------------------------------------------

cabe(caixa(_, W, H, D), container(ContainerW, ContainerH, ContainerD)) :-
    W =< ContainerW, 
    H =< ContainerH, 
    D =< ContainerD.

sobrepoe(_, []) :- false.
sobrepoe(alocada(_, X, Y, Z, W, H, D), [alocada(_, X2, Y2, Z2, W2, H2, D2) | _]) :-
    X + W > X2, X < X2 + W2,
    Y + H > Y2, Y < Y2 + H2,
    Z + D > Z2, Z < Z2 + D2.
sobrepoe(NovaCaixa, [_ | Restante]) :-
    sobrepoe(NovaCaixa, Restante).

pode_alocar(Caixa, Alocadas, Container) :-
    cabe(Caixa, Container),
    \+ sobrepoe(Caixa, Alocadas).
