% ---------------------------------------------------------
% PARTE 1 - entrada do problema
% ---------------------------------------------------------

% container(Width, Height, Depth)
container(20, 20, 20).

% Cada caixa segue o formato
% (Id, Width, Height, Depth).
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


% Empacotar caixas
empacotar(Resultado) :-
    findall(caixa(ID, W, H, D), caixa(ID, W, H, D), Caixas), % Lista de caixas
    container(CW, CH, CD),                                   % Dimensões do container
    empacotar_simples(Caixas, CW, CH, CD, [], Resultado).

% Caso base: sem caixas para empacotar
empacotar_simples([], _, _, _, Empacotadas, Empacotadas).

% Tenta empacotar uma caixa na próxima posição disponível
empacotar_simples([caixa(ID, W, H, D) | Resto], CW, CH, CD, Empacotadas, Resultado) :-
    soma_dimensoes(Empacotadas, TotalW, TotalH, TotalD),    % Calcula o espaço já ocupado
    TotalW + W =< CW, TotalH + H =< CH, TotalD + D =< CD,  % Verifica se a caixa cabe no container
    empacotar_simples(Resto, CW, CH, CD,
        [empacotada(ID, W, H, D) | Empacotadas], Resultado). % Adiciona a caixa
empacotar_simples([_ | Resto], CW, CH, CD, Empacotadas, Resultado) :-
    empacotar_simples(Resto, CW, CH, CD, Empacotadas, Resultado). % Ignora a caixa se não couber

% Soma as dimensões das caixas já empacotadas (simplificado para este exemplo)
soma_dimensoes([], 0, 0, 0).
soma_dimensoes([empacotada(_, W, H, D) | Resto], TotalW, TotalH, TotalD) :-
    soma_dimensoes(Resto, SubW, SubH, SubD),
    TotalW is SubW + W,
    TotalH is SubH + H,
    TotalD is SubD + D.

% Exemplo de execução
empacotamento :-
    empacotar(Resultado),
    write('Empacotamento: '), nl,
    write(Resultado), nl.
