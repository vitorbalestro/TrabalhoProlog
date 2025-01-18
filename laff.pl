% ---------------------------------------------------------
% PARTE 1 - entrada do problema
% ---------------------------------------------------------

% container(Width, Height, Depth)
container(20, 20, 20).

% Cada box segue o formato
% (Id, Width, Height, Depth).
boxes([
    box(1, 5, 5, 5),
    box(2, 10, 8, 6),
    box(3, 8, 7, 4),
    box(4, 12, 10, 8),
    box(5, 15, 12, 10),
    box(6, 7, 6, 5),
    box(7, 6, 6, 6),
    box(8, 9, 7, 5),
    box(9, 10, 10, 10),
    box(10, 12, 8, 8),
    box(11, 5, 5, 4),
    box(12, 6, 8, 6),
    box(13, 7, 5, 5),
    box(14, 8, 8, 6),
    box(15, 9, 10, 9),
    box(16, 14, 12, 10),
    box(17, 11, 9, 8),
    box(18, 13, 11, 9),
    box(19, 10, 9, 8),
    box(20, 6, 5, 5),
    box(21, 8, 7, 7),
    box(22, 9, 8, 8),
    box(23, 15, 14, 12),
    box(24, 16, 12, 10),
    box(25, 10, 10, 10),
    box(26, 6, 6, 6),
    box(27, 7, 6, 5),
    box(28, 9, 8, 7),
    box(29, 12, 10, 9),
    box(30, 14, 14, 12)
]).

% ---------------------------------------------------------
% Ordenação por maior área de base
% ---------------------------------------------------------

% Predicado usado pelo predsort para comparar áreas de base (Width * Depth).
compare_box_area(Order, box(_, W1, _, D1), box(_, W2, _, D2)) :-
    Area1 is W1 * D1,
    Area2 is W2 * D2,
    compare(RevOrder, Area1, Area2),
    reverse_order(RevOrder, Order).


% Predicados usados para ajustar o método compare
reverse_order('<', '>').
reverse_order('>', '<').
reverse_order('=', '<').

% ---------------------------------------------------------
% Algoritmo LAFF: Empacotamento
% ---------------------------------------------------------

% Predicado principal para o algoritmo LAFF.
pack_boxes_laff(_, [], PlacedBoxes, PlacedBoxes).

pack_boxes_laff(Container, [Box|RemainingBoxes], CurrentPlaced, FinalPlaced) :-
    % Tenta colocar a caixa na melhor posição disponível
    laff_place_box(Container, Box, CurrentPlaced, NewPlaced),
    pack_boxes_laff(Container, RemainingBoxes, NewPlaced, FinalPlaced).

pack_boxes_laff(Container, [_|RemainingBoxes], CurrentPlaced, FinalPlaced) :-
    % Caso não consiga colocar a caixa, continua com as próximas
    pack_boxes_laff(Container, RemainingBoxes, CurrentPlaced, FinalPlaced).

% ---------------------------------------------------------
% Busca pela melhor posição para uma caixa
% ---------------------------------------------------------

laff_place_box(Container, Box, PlacedBoxes, [PlacedBox|PlacedBoxes]) :-
    Container = container(Cw, Ch, Cd),
    
    % Gera as rotações possíveis da caixa
    rotations(Box, Rotations),
    member(box(Id, W, H, D), Rotations),
    
    % Gera todas as posições possíveis
    findall(
        placed_box(Id, W, H, D, X, Y, Z),
        (
            between(1, Cw, X),
            between(1, Ch, Y),
            between(1, Cd, Z),
            can_place_box(Container, placed_box(Id, W, H, D, X, Y, Z), PlacedBoxes)
        ),
        PossiblePlacements
    ),

    % Seleciona a posição mais "promissora" (e.g., menor altura Z)
    sort(7, @=<, PossiblePlacements, [PlacedBox|_]).

% ---------------------------------------------------------
% Geração de rotações para caixas
% ---------------------------------------------------------

% Gera as 6 rotações possíveis para uma caixa (W, H, D)
rotations(box(Id, W, H, D), [box(Id, W, H, D), box(Id, W, D, H), box(Id, H, W, D), box(Id, H, D, W), box(Id, D, W, H), box(Id, D, H, W)]).

% ---------------------------------------------------------
% Verificação de sobreposição
% ---------------------------------------------------------

overlaps(_, []) :- 
    false.

overlaps(placed_box(_, W1, H1, D1, X1, Y1, Z1), [placed_box(_, W2, H2, D2, X2, Y2, Z2)|_]) :-
    X1_max is X1 + W1 - 1,
    Y1_max is Y1 + H1 - 1,
    Z1_max is Z1 + D1 - 1,
    X2_max is X2 + W2 - 1,
    Y2_max is Y2 + H2 - 1,
    Z2_max is Z2 + D2 - 1,
    X1_max >= X2, X2_max >= X1,
    Y1_max >= Y2, Y2_max >= Y1,
    Z1_max >= Z2, Z2_max >= Z1.

overlaps(Box, [_|T]) :-
    overlaps(Box, T).

% ---------------------------------------------------------
% Verificação de viabilidade de colocação
% ---------------------------------------------------------

can_place_box(container(Cw, Ch, Cd), placed_box(_, W, H, D, X, Y, Z), PlacedBoxes) :-
    X > 0,
    Y > 0,
    Z > 0,
    X + W - 1 =< Cw,
    Y + H - 1 =< Ch,
    Z + D - 1 =< Cd,
    \+ overlaps(placed_box(_, W, H, D, X, Y, Z), PlacedBoxes).

% ---------------------------------------------------------
% Solução utilizando LAFF
% ---------------------------------------------------------

solve_laff :-
    % Carrega dados do container e caixas
    container(Cw, Ch, Cd),
    boxes(Boxes),

    % Ordena as caixas pela área de base
    predsort(compare_box_area, Boxes, SortedBoxes),

    % Aplica o algoritmo LAFF
    pack_boxes_laff(container(Cw, Ch, Cd), SortedBoxes, [], FinalPlaced),

    % Exibe os resultados
    display_result(FinalPlaced),
    !.

% ---------------------------------------------------------
% Exibição dos resultados
% ---------------------------------------------------------

display_result([]) :-
    write('Nenhuma caixa foi colocada.'), nl.

display_result(Placed) :-
    write('Caixas colocadas:'), nl,
    display_boxes(Placed).

display_boxes([]).
display_boxes([placed_box(Id, W, H, D, X, Y, Z)|T]) :-
    format('Caixa com id ~w de ~w x ~w x ~w em (~w, ~w, ~w)~n', [Id, W, H, D, X, Y, Z]),
    display_boxes(T).
