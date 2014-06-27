-module(functionUsagesInSingleFile).

-export([xxx/0]). % 1

'xxx<caret>'() ->
  xxx(), % 2
  'xxx'(), % 3
  functionUsagesInSingleFile:xxx(). % 4