-module('UnresolvedMacrosQuotedPredefined').
-export([f/0]).
f() -> {?'MODULE', ?'MODULE_STRING', ?'FILE', ?'LINE', ?'MACHINE', ?'OTP_RELEASE',
        <error descr="Unresolved macros '?'FUNCTION_NAME''">?'FUNCTION_NAME'</error>, <error descr="Unresolved macros '?'FUNCTION_ARITY''">?'FUNCTION_ARITY'</error>}.
