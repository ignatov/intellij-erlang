-module(bindBitSyntaxMacro).

%%%
%%% This test should check if "= <<" is treated as '=' and '<<' tokens, not as '=<' '<' after macro body substitution.
%%% See http://www.erlang.org/doc/programming_examples/bit_syntax.html chapter 4.2
%%%
-define(X_BYTE_0, X = <<0:8>>).

foo() -> ?X_BYTE_0, X.