# Descrição do problema

No problema do container, temos  um container com 3 dimensões (width, height e depth) e N caixas de entrada, também com 3 dimensões. Na nossa variação do problema, o objetivo é maximizar o número de caixas que entram no container, sem ponderação (ou seja, sem valores variáveis para cada caixa). Para simplificar o escopo do problema, não vamos considerar rotações de caixas, ou seja, uma caixa com as dimensões (1, 2, 2) e outra caixa com as dimensões (2, 2, 1) são na verdade caixas diferentes.

Para solução do problema, o grupo optou por utilizar a heurística de "First Fit Decreasing".
