-module(remoteCallNoExport).

foo() ->
  remoteCallNoExport_target:lll<caret>().
