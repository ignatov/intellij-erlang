-module(mymod).

-export([ foo/0
        , bar/0
        , baz/0
        ]).

-record(foo,
        {baz = 0
        }).

-define(LPAREN, ().
-define(RPAREN, )).

-define(ARROW, ->).
-define(COLON, :).
-define(COMMA, ,).

-define(LBRACKET, [).
-define(RBRACKET, ]).

-define(LCURLY, {).
-define(RCURLY, }).

-define(macro2(X, Y), {a,X,b,Y).

foo ?LPAREN ?RPAREN ?ARROW
  io ?COLON format ?LPAREN "Hello" ?COMMA ?LBRACKET ?RBRACKET ?RPAREN ?COMMA
  io ?COLON format ?LPAREN " " ?COMMA ?LBRACKET ?RBRACKET ?RPAREN ?COMMA
  io ?COLON format ?LPAREN "World~n" ?COMMA ?LBRACKET ?RBRACKET ?RPAREN
  .

-define(increment_rec(Record, Field, Val), Record{Field=Record.Field + (Val)}).

bar() ->
  Foo = #foo{},
  ?increment_rec(Foo#foo, baz, 1).

baz() ->
  ?macro2(x,y)}.
