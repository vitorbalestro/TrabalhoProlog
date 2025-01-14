% ---------------------------------------------------------
% PARTE 1 - entrada do problema
% ---------------------------------------------------------

% container(Width, Height, Depth)
container(10, 10, 10).

% Cada box segue o mesmo formato do
% container: (Width, Height, Depth).
boxes([
    box(2, 2, 2),
    box(1, 1, 1),
    box(3, 3, 3)
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
reverse_order('=', '=').


% ---------------------------------------------------------
% PARTE 3 - critérios de aceite de uma caixa
% ---------------------------------------------------------



% ---------------------------------------------------------
% PARTE N - solução
% - ordena as caixas pelo volume
% - ...?
% ---------------------------------------------------------


solve(SortedBoxes) :-
    boxes(Boxes),
    predsort(compare_box_volume, Boxes, SortedBoxes).
