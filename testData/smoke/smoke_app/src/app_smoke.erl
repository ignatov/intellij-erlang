-module(app_smoke).
-export([main/2]).

main(Text, Term) ->
    io:format("TEXT=~p TERM=~p APP_ENV=~p SMOKE=~p~n", [Text, Term, os:getenv("APP_ENV"), init:get_argument(smoke)]).
