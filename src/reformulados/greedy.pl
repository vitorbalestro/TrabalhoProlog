:- ensure_loaded('./utils/input.pl').
:- ensure_loaded('./utils/write_result.pl').

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

can_place_box(placed_box(_, W, H, D, X, Y, Z), PlacedBoxes):-
    container(Cw, Ch, Cd),
    X > 0,
    Y > 0,
    Z > 0,
    X + W - 1 =< Cw,
    Y + H - 1 =< Ch,
    Z + D - 1 =< Cd,
    \+ overlaps(placed_box(_, W, H, D, X, Y, Z), PlacedBoxes).

reverse_order('<', '>').
reverse_order('>', '<').
reverse_order('=', '<').
compare_box_volume(Order, box(_, W1, H1, D1), box(_, W2, H2, D2)) :-
    Volume1 is W1 * H1 * D1,
    Volume2 is W2 * H2 * D2,
    compare(RevOrder, Volume1, Volume2),
    reverse_order(RevOrder, Order).

% Se não tem caixa pra empacotar, mantém a lista de empacotadas
pack([], PackedBoxes, PackedBoxes).
% se não consegue empacotar uma caixa, pula e vai pra próxima
pack([Box|RestBoxes], CurrentPacked, FinalPacked):-
    \+ box_position(Box, CurrentPacked, _),
    pack(RestBoxes, CurrentPacked, FinalPacked).
% se consegue empacotar uma caixa, empacota e vai pra próxima
pack([Box|RestBoxes], CurrentPacked, FinalPacked) :-
    box_position(Box, CurrentPacked, NewBox),
    pack(RestBoxes, [NewBox|CurrentPacked], FinalPacked).

box_position(box(Id, W, H, D), PlacedBoxes, PlacedBox) :-
    container(Cw, Ch, Cd),
    between(0, Cw, X),
    between(0, Ch, Y),
    between(0, Cd, Z),
    PlacedBox = placed_box(Id, W, H, D, X, Y, Z),
    can_place_box(PlacedBox, PlacedBoxes).

solve :-
    findall(box(Id, W, H, D), box(Id, W, H, D), Boxes),
    predsort(compare_box_volume, Boxes, SortedBoxes),
    pack(SortedBoxes, [], PackedBoxes),
    write_result(PackedBoxes),
    !.
