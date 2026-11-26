-module('ControlEscapeAnyCharOtp25').
-export([s1/0, s2/0, s3/0, s4/0, s5/0, s6/0, ok/0]).

s1() -> "\^1".
s2() -> "\^ ".
s3() -> "\^"".
s4() -> "a\^"b".
s5() -> "\^"" ++ "b".
s6() -> "\^\".

ok() -> ok.
