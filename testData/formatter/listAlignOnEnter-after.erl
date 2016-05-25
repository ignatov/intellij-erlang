test() ->
  case ets:new(?TAB, [set, named_table, public,
                      <caret>