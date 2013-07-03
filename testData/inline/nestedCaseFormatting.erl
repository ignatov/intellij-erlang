test() ->
  CaseN = case 1 of
            Z -> Z
          end,
  case <caret>CaseN of
    Z -> Z
  end.