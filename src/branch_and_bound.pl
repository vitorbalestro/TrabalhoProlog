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
% PARTE 3 - Encontrar posição válida dentro do container
% ---------------------------------------------------------

sobrepoe(_, []) :- false.
sobrepoe(alocada(_, X, Y, Z, W, H, D), [alocada(_, X2, Y2, Z2, W2, H2, D2) | _]) :-
    X + W > X2, X < X2 + W2,
    Y + H > Y2, Y < Y2 + H2,
    Z + D > Z2, Z < Z2 + D2.
sobrepoe(NovaCaixa, [_ | Restante]) :-
    sobrepoe(NovaCaixa, Restante).

posicao_valida(W, H, D, Alocadas, container(ContainerW, ContainerH, ContainerD), X, Y, Z) :-
    between(0, ContainerW, X),
    between(0, ContainerH, Y),
    between(0, ContainerD, Z),
    X + W =< ContainerW,
    Y + H =< ContainerH,
    Z + D =< ContainerD,
    \+ sobrepoe(alocada(_, X, Y, Z, W, H, D), Alocadas).

% ---------------------------------------------------------
% PARTE 4 - Inicializa lista com cada caixa alocada em posição válida
% ---------------------------------------------------------

inicializa_lista_caixas(Caixas, Container, ListaInicial) :-
    findall([Custo, [alocada(Id, X, Y, Z, W, H, D)]],
        (
            member(caixa(Id, W, H, D), Caixas),
            posicao_valida(W, H, D, [], Container, X, Y, Z),
            volume(W, H, D, Custo)
        ),
        ListaInicial).

% ---------------------------------------------------------
% PARTE 5 - Ordena lista pelo custo
% ---------------------------------------------------------

ordena(Lista, ListaOrdenada) :-
    sort(1, @=<, Lista, ListaOrdenada).
