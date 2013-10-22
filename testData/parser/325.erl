fact() ->
    fun Fact(N) when N > 0 -> N * Fact(N - 1); Fact(0) -> 1 end.