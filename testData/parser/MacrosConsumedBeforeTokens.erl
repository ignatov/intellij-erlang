-module('MacrosConsumedBeforeTokens').

-define(LPAREN, ().
-define(RPAREN, )).

-define(LBRACKET, [).
-define(RBRACKET, ]).

-define(LCURLY, {).
-define(RCURLY, }).

-define(ARROW, ->).
-define(COLON, :).
-define(SEMICOLON, ;).
-define(COMMA, ,).
-define(HASH, #).

-record(test_record,{}).

foo?LPAREN 1?RPAREN ?ARROW
  ?HASH test_record ?LCURLY ?RCURLY ?COMMA
  io ?COLON format ?LPAREN "~n" ?RPAREN ?COMMA
  ?LBRACKET ok ?RBRACKET ?SEMICOLON
foo(_) -> ok.


