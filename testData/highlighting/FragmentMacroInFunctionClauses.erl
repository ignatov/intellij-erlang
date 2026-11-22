-export([handle_call/3]).
-define(REPLY(Value), {reply, Value,).
handle_call(get, _From, State) ->
  ?REPLY(State) State};
handle_call(bump, _From, State) ->
  ?REPLY(ok) State};
handle_call(_Other, _From, State) ->
  {reply, {error, unknown}, State}.
