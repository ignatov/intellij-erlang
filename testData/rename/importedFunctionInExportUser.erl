-module(importedFunctionInExportUser).

-import(importedFunctionInExport, [xxx/0]).
-export([xxx/0]).

foo() -> xxx().
