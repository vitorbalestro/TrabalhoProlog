% Dimensões do container
container(10, 10, 10).

% Lista de caixas (ID, largura, altura, profundidade)
caixa(1, 5, 5, 5).
caixa(2, 4, 4, 4).
caixa(3, 3, 3, 3).
caixa(4, 2, 2, 2).

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
