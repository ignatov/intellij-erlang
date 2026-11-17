-module(importedFunctionUser).

-import(importedFunctionUsages, [xxx/0]). % 2
-export([xxx/0]). % not a usage: an export entry names a function of its own module

foo() -> xxx(). % 3
