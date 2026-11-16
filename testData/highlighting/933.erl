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

-define(TYPED, tid :: id()).
-define(TYPED_EXT, ?TYPED, textra :: term()).
-record(typed_ext, {?TYPED_EXT, tz}).

-define(PLAIN, pid).
-define(PLAIN_EXT, ?PLAIN, pextra).
-record(plain_ext, {?PLAIN_EXT, pz}).

-define(PLAIN_DEFAULT, ?PLAIN = 1, q).
-record(plain_default, {?PLAIN_DEFAULT}).

test() -> {#button{id=button, text = "submit"}, #link{id=link, postback=click, style="", url = "/"}, #link{<error descr="Unresolved record field 'missing'">missing</error>=1},
  #typed_ext{tid = a, textra = 1, tz = 2}, #plain_ext{pid = a, pextra = 1, pz = 2},
  #plain_default{pid = a, q = 1}}.
