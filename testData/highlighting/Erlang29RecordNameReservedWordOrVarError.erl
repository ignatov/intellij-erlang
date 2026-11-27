-export([f/0, g/1, h/1]).
-record('when', {a}).
-record('Foo', {b}).
-record(plain, {c}).

%% OTP 29 record names should report errors on SDK < 29
f() -> {#<error descr="Record names that are reserved words or variables are only supported in Erlang 29 and newer versions">when</error>{a = 1}, #<error descr="Record names that are reserved words or variables are only supported in Erlang 29 and newer versions">Foo</error>{b = 2}, #'Foo'{b = 3}, #plain{c = 4}}.
g(#<error descr="Record names that are reserved words or variables are only supported in Erlang 29 and newer versions">when</error>{a = A}) -> A.
h(R) -> {R#<error descr="Record names that are reserved words or variables are only supported in Erlang 29 and newer versions">Foo</error>.b, R#<error descr="Record names that are reserved words or variables are only supported in Erlang 29 and newer versions">Foo</error>{b = 1}, R#'when'.a, R#plain.c}.
