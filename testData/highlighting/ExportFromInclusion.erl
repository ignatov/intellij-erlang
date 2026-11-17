-include("testapp-1/include/recursive.hrl").
-include("testapp-1/include/transitiveImports.hrl").

-export([resolved_function/0, <error descr="Unresolved function foo/0">foo/0</error>]).
