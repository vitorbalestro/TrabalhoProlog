% container(Width, Height, Depth)
container(20, 20, 20).

% Cada caixa segue o formato
% (Id, Width, Height, Depth).
box(1, 5, 5, 5).
box(2, 10, 8, 6).
box(3, 8, 7, 4).
box(4, 12, 10, 8).
box(5, 15, 12, 10).
box(6, 7, 6, 5).
box(7, 6, 6, 6).
box(8, 9, 7, 5).
box(9, 10, 10, 10).
box(10, 12, 8, 8).
box(11, 5, 5, 4).
box(12, 6, 8, 6).
box(13, 7, 5, 5).
box(14, 8, 8, 6).
box(15, 9, 10, 9).
box(16, 14, 12, 10).
box(17, 11, 9, 8).
box(18, 13, 11, 9).
box(19, 10, 9, 8).
box(20, 6, 5, 5).
box(21, 8, 7, 7).
box(22, 9, 8, 8).
box(23, 15, 14, 12).
box(24, 16, 12, 10).
box(25, 10, 10, 10).
box(26, 6, 6, 6).
box(27, 7, 6, 5).
box(28, 9, 8, 7).
box(29, 12, 10, 9).
box(30, 14, 14, 12).

fits(container(WC, HC, DC), (X, Y, Z), box(_, WB, HB, DB)):-
    X > 0,
    Y > 0,
    Z > 0,
    X + WB - 1 =< WC,
    Y + HB - 1 =< HC,
    Z + DB - 1 =< DC.

overlaps((X1, Y1, Z1), box(_, W1, H1, D1), (X2, Y2, Z2), box(_, W2, H2, D2)):-
    X1_max is X1 + W1,
    Y1_max is Y1 + H1,
    Z1_max is Z1 + D1,
    X2_max is X2 + W2,
    Y2_max is Y2 + H2,
    Z2_max is Z2 + D2,
    X1_max > X2, X2_max > X1,
    Y1_max > Y2, Y2_max > Y1,
    Z1_max > Z2, Z2_max > Z1.

solve:-
    findall(box(Id, W, H, D), box(Id, W, H, D), Boxes),
    write(Boxes),
    nl.
