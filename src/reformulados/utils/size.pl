solution_size([], 0).
solution_size([_|Rest], Size):-
    solution_size(Rest, RestSize),
    Size is 1 + RestSize.
