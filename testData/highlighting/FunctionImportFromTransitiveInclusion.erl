-module('FunctionImportFromTransitiveInclusion').

-include_lib("testapp-1/include/transitiveImports.hrl").

-export([bar/0]).

bar() -> foo().