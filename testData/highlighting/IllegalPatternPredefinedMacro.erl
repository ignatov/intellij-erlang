-export([bad/1, ok/1, user/1]).
-define(PREFIX, "abc").
-define(CH, $a).

bad(<error>?MODULE ++ Rest</error>) -> Rest;
bad(<error>?LINE ++ Rest</error>) -> Rest;
bad(<error>?FUNCTION_NAME ++ Rest</error>) -> Rest;
bad(<error>?FUNCTION_ARITY ++ Rest</error>) -> Rest;
bad(<error>?MACHINE ++ Rest</error>) -> Rest;
bad(<error>(?MODULE) ++ Rest</error>) -> Rest;
bad(<error>[?MODULE] ++ Rest</error>) -> Rest;
bad(<error>[?FUNCTION_NAME] ++ Rest</error>) -> Rest;
bad(<error>[?MACHINE] ++ Rest</error>) -> Rest;
bad(<error>[?MODULE_STRING] ++ Rest</error>) -> Rest;
bad(<error>[?FILE] ++ Rest</error>) -> Rest;
bad(<error>[$a | ?MODULE] ++ Rest</error>) -> Rest;
bad(<error>[$a | ?LINE] ++ Rest</error>) -> Rest;
bad(X) -> <error>?MODULE ++ R</error> = X, R.

ok(?MODULE_STRING ++ Rest) -> Rest;
ok(?FILE ++ Rest) -> Rest;
ok("a" ?MODULE_STRING ++ Rest) -> Rest;
ok([?LINE] ++ Rest) -> Rest;
ok([?FUNCTION_ARITY] ++ Rest) -> Rest;
ok([$a | ?FILE] ++ Rest) -> Rest.

% Known limitation: user macros are not expanded, so these are not reported although erlc rejects them
user(?CH ++ Rest) -> Rest;
user([?PREFIX] ++ Rest) -> Rest.
