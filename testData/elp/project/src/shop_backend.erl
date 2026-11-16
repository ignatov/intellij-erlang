-module(shop_backend).

-export([lookup/2]).
-export_type([key/0]).

-type key() :: atom().

-callback fetch(Key :: key()) -> {ok, term()} | error.

%% @doc Looks up Key in the backend module Backend.
-spec lookup(module(), key()) -> {ok, term()} | error.
lookup(Backend, Key) ->
  Backend:fetch(Key).
