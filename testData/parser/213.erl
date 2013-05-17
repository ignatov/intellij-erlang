-module(test).

-record(on_resolve_denied, {
    request :: #resolve_request{} | string(),
    alternative = undefined :: symbol_info_type()
}).
