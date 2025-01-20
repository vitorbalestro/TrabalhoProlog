% ---------------------------------------------------------
% PARTE 1 - Entrada do problema.
% container(Width, Height, Depth)
% caixa(Id, Width, Height, Depth).
% ---------------------------------------------------------

container(50,50,50).

caixa(1, 5, 5, 5).
caixa(2, 10, 8, 6).
caixa(3, 8, 7, 4).
caixa(4, 12, 10, 8).
caixa(5, 15, 12, 10).
caixa(6, 7, 6, 5).
caixa(7, 6, 6, 6).
caixa(8, 9, 7, 5).
caixa(9, 10, 10, 10).
caixa(10, 12, 8, 8).
caixa(11, 5, 5, 4).
caixa(12, 6, 8, 6).
caixa(13, 7, 5, 5).
caixa(14, 8, 8, 6).
caixa(15, 9, 10, 9).
caixa(16, 14, 12, 10).
caixa(17, 11, 9, 8).
caixa(18, 13, 11, 9).
caixa(19, 10, 9, 8).
caixa(20, 6, 5, 5).
caixa(21, 8, 7, 7).
caixa(22, 9, 8, 8).
caixa(23, 15, 14, 12).
caixa(24, 16, 12, 10).
caixa(25, 10, 10, 10).
caixa(26, 6, 6, 6).
caixa(27, 7, 6, 5).
caixa(28, 9, 8, 7).
caixa(29, 12, 10, 9).
caixa(30, 14, 14, 12).

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
% PARTE 3 - Ordena pelo custo (volume) decrescente.
% ---------------------------------------------------------

adicionar_volume(caixa(Id, W, H, D), caixa(Id, W, H, D)-Volume) :-
    volume(W, H, D, Volume).

ordena(Caixas, CaixasOrdenadas) :-
    maplist(adicionar_volume, Caixas, CaixasComVolume),
    sort(2, @>=, CaixasComVolume, CaixasOrdenadasComVolume),
    pairs_keys(CaixasOrdenadasComVolume, CaixasOrdenadas).

% ---------------------------------------------------------
% PARTE 4 - Encontrar posição válida dentro do container
% ---------------------------------------------------------

no_container(X, Y, Z, ContainerW, ContainerH, ContainerD) :-
    X + W =< ContainerW,
    Y + H =< ContainerH,
    Z + D =< ContainerD.

sobrepoe(_, []) :- false.
sobrepoe(alocada(_, X, Y, Z, W, H, D), [alocada(_, X2, Y2, Z2, W2, H2, D2) | _]) :-
    X + W > X2, X < X2 + W2,
    Y + H > Y2, Y < Y2 + H2,
    Z + D > Z2, Z < Z2 + D2.
sobrepoe(NovaCaixa, [_ | Restante]) :-
    sobrepoe(NovaCaixa, Restante).

posicao_valida(X, Y, Z, Alocadas) :-
    container(ContainerW, ContainerH, ContainerD),
    no_container(X, Y, Z, ContainerW, ContainerH, ContainerD),
    \+ sobrepoe(alocada(_, X, Y, Z, W, H, D), Alocadas).

% ---------------------------------------------------------
% PARTE 5 - Branch and Bound
% ---------------------------------------------------------

% branch_and_bound(CaixasOrdenadas, Posicoes, MelhorSolucao, MelhorCusto) :-

% ---------------------------------------------------------
% PARTE 6 - Executa
% ---------------------------------------------------------

solve :-
    findall(caixa(Id, W, H, D), caixa(Id, W, H, D), Caixas),
    ordena(Caixas, CaixasOrdenadas),
    % findall(),
    % branch_and_bound(),
    write('Melhor solução encontrada:'), nl,
    write(MelhorSolucao), nl,
    write('Custo total (volume): '), write(MelhorCusto), nl.