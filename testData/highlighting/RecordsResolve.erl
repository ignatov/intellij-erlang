-export([f/1]).
-record(name1234, {id}).
f(A) -> A#name1234{}, A#<error>dummy</error>{}.