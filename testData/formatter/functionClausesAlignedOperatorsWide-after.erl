handle(some_pattern) -> ok;
handle(X) -> A = X + 1, B = A * 2, C = B - 3, D = C + 4, E = D + 5, F = E + 6, G = F + 7, H = G + 8,
  {A, B, C, D, E, F, G, H}.
