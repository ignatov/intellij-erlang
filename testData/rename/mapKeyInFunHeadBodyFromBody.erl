f(K, L) -> lists:map(fun(#{K := V}) -> {K<caret>, V} end, L).
