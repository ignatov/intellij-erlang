-module(remoteCall).

foo() ->
  remoteCall_target:lll<caret>(1, aaaa, OOO).
