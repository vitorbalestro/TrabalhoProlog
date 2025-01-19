% ---------------------------------------------------------
% PARTE 1 - entrada do problema
% ---------------------------------------------------------

% container(Width, Height, Depth)
container(10, 10, 10).

% Cada box segue o mesmo formato do
% container: (Width, Height, Depth).
boxes([
    box(2, 3, 5),
    box(1, 2, 4),
    box(3, 4, 5)
]).

% ---------------------------------------------------------
% PARTE 2 - ordenação por volume
% ---------------------------------------------------------


% Predicado usado pelo predsort para comparar volumes.
compare_box_volume(Order, box(W1, H1, D1), box(W2, H2, D2)) :-
    Volume1 is W1 * H1 * D1,
    Volume2 is W2 * H2 * D2,
    compare(RevOrder, Volume1, Volume2),
    reverse_order(RevOrder, Order).

% Predicados usados para ajustar o método compare
% para ordenar de forma decrescente pelo volume.
reverse_order('<', '>').
reverse_order('>', '<').
reverse_order('=', '<'). % fazemos isso para não retornar = nunca, pois o predsort remove itens duplicados


% ---------------------------------------------------------
% PARTE 3 - verificação de overlap com caixas no container
% ---------------------------------------------------------


overlaps(_, []) :-
    false.

overlaps(placed_box(W1, H1, D1, X1, Y1, Z1), [placed_box(W2, H2, D2, X2, Y2, Z2)|_]) :-
    X1_max is X1 + W1,
    Y1_max is Y1 + H1,
    Z1_max is Z1 + D1,
    X2_max is X2 + W2,
    Y2_max is Y2 + H2,
    Z2_max is Z2 + D2,
    X1_max > X2, X2_max > X1,
    Y1_max > Y2, Y2_max > Y1,
    Z1_max > Z2, Z2_max > Z1.

overlaps(Box, [_|T]) :-
    overlaps(Box, T).


% ---------------------------------------------------------
% PARTE 4 - verificação se pode colocar uma caixa no container
% ---------------------------------------------------------

can_place_box(container(Cw, Ch, Cd), placed_box(W, H, D, X, Y, Z), PlacedBoxes):-
    X > 0,
    Y > 0,
    Z > 0,
    X + W - 1 =< Cw,
    Y + H - 1 =< Ch,
    Z + D - 1 =< Cd,
    \+ overlaps(placed_box(W, H, D, X, Y, Z), PlacedBoxes).

% ---------------------------------------------------------
% PARTE 5 - coloca caixa em alguma posição
% ---------------------------------------------------------


place_box(Container, Box, PlacedBoxes, [placed_box(W, H, D, X, Y, Z)|PlacedBoxes]):-
    Container = container(Cw, Ch, Cd),
    Box = box(W, H, D),
    
    % Gera uma posição potencial (X, Y, Z)
    between(1, Cw, X),
    between(1, Ch, Y),
    between(1, Cd, Z),

    can_place_box(Container, placed_box(W, H, D, X, Y, Z), PlacedBoxes),
    !.

% ---------------------------------------------------------
% PARTE 6 - solução
% ---------------------------------------------------------

pack_boxes(_, [], PlacedBoxes, PlacedBoxes).

pack_boxes(Container, [_|RemainingBoxes], CurrentPlaced, FinalPlaced) :-
    pack_boxes(Container, RemainingBoxes, CurrentPlaced, FinalPlaced).

pack_boxes(Container, [Box|RemainingBoxes], CurrentPlaced, FinalPlaced) :-
    place_box(Container, Box, CurrentPlaced, NewPlaced),
    pack_boxes(Container, RemainingBoxes, NewPlaced, FinalPlaced).

solve :-
    % puxando dados da entrada
    container(Cw, Ch, Cd),
    boxes(Boxes),

    % ordena pelo volume
    predsort(compare_box_volume, Boxes, SortedBoxes),

    findall(PlacedBoxes, pack_boxes(container(Cw, Ch, Cd), SortedBoxes, [], PlacedBoxes), AllSolutions),

    best_solution(AllSolutions, BestSolution),

    display_result(BestSolution),
    !.

best_solution(Solutions, BestSolution) :-
    maplist(length, Solutions, Lengths),
    max_list(Lengths, MaxLength),
    nth0(Index, Lengths, MaxLength),
    nth0(Index, Solutions, BestSolution).

% ---------------------------------------------------------
% PARTE 7 - exibição da solução
% ---------------------------------------------------------

display_boxes([]).
display_boxes([placed_box(W, H, D, X, Y, Z)|T]):-
    format('Caixa ~w x ~w x ~w em (~w, ~w, ~w)~n', [W, H, D, X, Y, Z]),
    display_boxes(T).

display_result([]):-
    write('Nenhuma caixa foi colocada'), nl.
display_result(Placed):-
    write('Caixas colocadas:'),nl,
    display_boxes(Placed).
