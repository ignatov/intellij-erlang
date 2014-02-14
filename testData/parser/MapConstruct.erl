foo() ->
  #{key => value},
  #{key := value}, % although this is incorrect, parser should eat it.
  #{key1 => value, key2 => value},
  #{}.