%%  Copyright (c) 2012. Sergey Ignatov.
-module(empty).
-author("ignatov").

bar() -> ok.
foo<caret>() -> ok.

-spec bar() -> atom().
-spec foo() -> atom().