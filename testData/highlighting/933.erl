-export([test/0]).

-type id() :: atom().
-type class() :: string().
-type text() :: string().
-type body() :: string().

-define(ELEMENT_BASE(Module),
  module=Module           :: atom(),
  id                      :: id(),
  class=""                :: class() | [class()],
  style=""                :: text()
).

-record(button, {?ELEMENT_BASE(element_button),
  text = "" :: text(),
  body = "" :: body()
}).

test() -> #button{id=button, text = "submit"}.
