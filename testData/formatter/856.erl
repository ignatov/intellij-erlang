foo() ->
  _Str = lists:flatten("SELECT " ++ lists:join(", ", ["a", "b"]) ++
  " FROM " ++
  binary_to_list(<<"pubic">>) ++ "." ++ binary_to_list(<<"table">>) ++
  " WHERE " ++
  lists:join(" AND ", ["c = 1", "d = 2"])),
  PKeys = ["z", "y"],
  _PKColumns = lists:reverse(["column" ++ " := " ++
  Pkey || Pkey <- PKeys]),
  ok.
