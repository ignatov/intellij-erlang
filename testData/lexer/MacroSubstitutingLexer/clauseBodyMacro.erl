-module(clauseBodyMacro).

-define(CLAUSE_BODY, -> ok).

foo() ?CLAUSE_BODY.