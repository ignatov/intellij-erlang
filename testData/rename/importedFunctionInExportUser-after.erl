-module(importedFunctionInExportUser).

-import(importedFunctionInExport, [bar/0]).
-export([xxx/0]).

foo() -> bar().
