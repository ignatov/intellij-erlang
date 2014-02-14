foo() ->
  M = some_map,
  M1 = M#{key := value},
  M2 = M1#{key => value},
  M3 = M2#{key => value, key2 => value},
  #{}#{key => value}.