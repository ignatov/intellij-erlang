-module('recordExpression').

-record(rec, {field}).

foo(X) -> X#rec.field.