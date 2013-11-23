foo11() ->
  Value1 = 1,
  SomethingElse = 1,
  X = [{key1, Value1}
      , {zzz,   (mymodule:myfun())#myrecord.myfield}
                           , {bypass, true}
    |      SomethingElse    ].