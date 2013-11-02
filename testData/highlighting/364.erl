-export([foo/0]).

foo() ->
    F='CamelCase':f(),
    'Camel1Case':<warning>f</warning>(),
    'CamelCase':<warning>f1</warning>(),
    io:format("~p", [F]).