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

-define(BUTTON_BASE(Module), ?ELEMENT_BASE(Module), postback :: term()).

-record(button, {?ELEMENT_BASE(element_button),
  text = "" :: text(),
  body = "" :: body()
}).

-record(link, {?BUTTON_BASE(element_link), url = "" :: text()}).

test() -> {#button{id=button, text = "submit"}, #link{id=link, postback=click, style="", url = "/"}, #link{<error descr="Unresolved record field 'missing'">missing</error>=1}}.
