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
