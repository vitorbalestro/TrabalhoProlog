% Imprime a solução: lista de caixas embaladas
write_result([]) :-
    write('Nenhuma outra caixa foi embalada.'), nl.
write_result([placed_box(Id, W, H, D, X, Y, Z)|T]) :-
    format('Caixa ID: ~w, Largura: ~w, Altura: ~w, Profundidade: ~w, Posicao: (~w, ~w, ~w)', [Id, W, H, D, X, Y, Z]), nl,
    write_result(T).
