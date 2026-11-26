-module('LoneDot').
-export([f/0]).
-record(r, {a}). <error descr="Unexpected '.'">.</error>
f() -> #r{}.
