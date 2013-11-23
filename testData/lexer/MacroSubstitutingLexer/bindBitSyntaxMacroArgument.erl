-module(bindBitSyntaxMacroArgument).

-define(IDENTITY(X), X).

foo() -> ?IDENTITY(A = <<0:8>>).