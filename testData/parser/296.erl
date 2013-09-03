%% Copyright
-module(test2).
-author("ignatov").

%% API
-export([]).

%% -include("inc.hrl"). %%The name of file is still green but not navigable by Ctrl+LMButton, "Cannot find declaration to go to"


-define(test_func_macro, test).
-define(TRUE, true).
-export([?test_func_macro/0, ?test_func_macro/0]). %%error

-define(a, 1).

another_func() ->
    ?TRUE. %%error

?test_func_macro() -> ok. %%error

?a().
?a().


foo1(0).


xxx(1) -> not_ok.

foao() -> ?test_func_macro().
foo() -> ok.

foo1() -> ok.

f() -> ?test_func_macro().    foo() -> ok .