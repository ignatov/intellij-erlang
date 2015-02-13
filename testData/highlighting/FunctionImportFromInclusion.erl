-module('FunctionImportFromInclusion').

-include_lib("testapp-1/include/imports.hrl").

-export([bar/0]).

bar() -> foo().