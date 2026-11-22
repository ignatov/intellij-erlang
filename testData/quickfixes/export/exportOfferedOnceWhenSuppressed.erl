-module(exp_suppressed).
-export([used/0]).

used() -> ok.

%% noinspection ErlangUnusedFunction
unused<caret>_one() -> ok.
