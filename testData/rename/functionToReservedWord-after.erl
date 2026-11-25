-module(functionToReservedWord).
-export(['div'/0]).

'div'() ->
  'div'(),
  functionToReservedWord:'div'().
