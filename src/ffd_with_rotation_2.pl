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
    box(15, 9, 10, 9)
]).

% ---------------------------------------------------------
% PARTE 2 - ordenação por volume
% ---------------------------------------------------------

% Predicado usado pelo predsort para comparar volumes.
compare_box_volume(Order, box(_, W1, H1, D1), box(_, W2, H2, D2)) :-
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

overlaps(placed_box(_, W1, H1, D1, X1, Y1, Z1), [placed_box(_, W2, H2, D2, X2, Y2, Z2)|_]) :-
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

can_place_box(container(Cw, Ch, Cd), placed_box(_, W, H, D, X, Y, Z), PlacedBoxes) :-
    X > 0,
    Y > 0,
    Z > 0,
    X + W - 1 =< Cw,
    Y + H - 1 =< Ch,
    Z + D - 1 =< Cd,
    \+ overlaps(placed_box(_, W, H, D, X, Y, Z), PlacedBoxes).

% ---------------------------------------------------------
% PARTE 5 - gera as rotações possíveis de uma caixa
% ---------------------------------------------------------

% Gera as 6 rotações possíveis para uma caixa (W, H, D)
rotations(box(Id, W, H, D), [box(Id, W, H, D), box(Id, W, D, H), box(Id, H, W, D), box(Id, H, D, W), box(Id, D, W, H), box(Id, D, H, W)]).

% ---------------------------------------------------------
% PARTE 6 - coloca caixa em alguma posição
% ---------------------------------------------------------

place_box(Container, Box, PlacedBoxes, [placed_box(Id, W, H, D, X, Y, Z)|PlacedBoxes]) :-
    Container = container(Cw, Ch, Cd),
    
    % Gera as rotações possíveis da caixa
    rotations(Box, Rotations),
    member(box(Id, W, H, D), Rotations),
    
    % Gera uma posição potencial (X, Y, Z)
    between(1, Cw, X),
    between(1, Ch, Y),
    between(1, Cd, Z),

    can_place_box(Container, placed_box(Id, W, H, D, X, Y, Z), PlacedBoxes),
    !.

% ---------------------------------------------------------
% PARTE 7 - solução
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
% PARTE 8 - exibição da solução
% ---------------------------------------------------------

display_boxes([]).
display_boxes([placed_box(Id, W, H, D, X, Y, Z)|T]):-
    format('Caixa com id ~w de ~w x ~w x ~w em (~w, ~w, ~w)~n', [Id, W, H, D, X, Y, Z]),
    display_boxes(T).

display_result([]):-
    write('Nenhuma caixa foi colocada'), nl.
display_result(Placed):-
    write('Caixas colocadas:'),nl,
    display_boxes(Placed),
    nl,
    display_minecraft_colored_commands(Placed).

% ---------------------------------------------------------
% PARTE 9 - comandos para preencher as caixas no minecraft
% para melhor visualização da solução
% ---------------------------------------------------------

wool_colors([orange_wool, magenta_wool, light_blue_wool,
             yellow_wool, lime_wool, pink_wool, gray_wool,
             light_gray_wool, cyan_wool, purple_wool, blue_wool,
             brown_wool, green_wool, red_wool]).

box_color(Index, Color) :- 
    wool_colors(Colors),
    Length is Index mod 14,
    nth0(Length, Colors, Color).

generate_colored_fill_commands(PlacedBoxes) :-
    findall(_, (nth0(Index, PlacedBoxes, PlacedBox), 
               box_color(Index, Color),
               print_colored_fill_command(PlacedBox, Color)), _).

print_colored_fill_command(placed_box(_, W, H, D, X, Y, Z), Color) :-
    X2 is X + W - 1,
    Y2 is Y + H - 1,
    Z2 is Z + D - 1,
    format('/fill ~w ~w -~w ~w ~w -~w ~w~n', [X, Y, Z, X2, Y2, Z2, Color]).

display_minecraft_colored_commands(PlacedBoxes) :-
    generate_colored_fill_commands(PlacedBoxes).
