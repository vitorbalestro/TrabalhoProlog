:- ensure_loaded('./utils/input.pl').
:- ensure_loaded('./utils/write_result.pl').

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
pack_boxes_laff([], PlacedBoxes, PlacedBoxes).

pack_boxes_laff([Box|RemainingBoxes], CurrentPlaced, FinalPlaced) :-
    % Tenta colocar a caixa na melhor posição disponível
    laff_place_box(Box, CurrentPlaced, NewPlaced),
    pack_boxes_laff(RemainingBoxes, NewPlaced, FinalPlaced).

pack_boxes_laff([_|RemainingBoxes], CurrentPlaced, FinalPlaced) :-
    % Caso não consiga colocar a caixa, continua com as próximas
    pack_boxes_laff(RemainingBoxes, CurrentPlaced, FinalPlaced).

pack_boxes_laff(Boxes, Result):-
    pack_boxes_laff(Boxes, [], Result).

% ---------------------------------------------------------
% Busca pela melhor posição para uma caixa
% ---------------------------------------------------------

laff_place_box(Box, PlacedBoxes, [PlacedBox|PlacedBoxes]) :-
    container(Cw, Ch, Cd),
    
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
            can_place_box(placed_box(Id, W, H, D, X, Y, Z), PlacedBoxes)
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

can_place_box(placed_box(_, W, H, D, X, Y, Z), PlacedBoxes) :-
    container(Cw, Ch, Cd),
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

solve :-
    % Carrega dados das caixas
    findall(box(Id, W, H, D), box(Id, W, H, D), Boxes),

    % Ordena as caixas pela área de base
    predsort(compare_box_area, Boxes, SortedBoxes),

    % Aplica o algoritmo LAFF
    pack_boxes_laff(SortedBoxes, FinalPlaced),

    % Exibe os resultados
    write_result(FinalPlaced),
    !.
