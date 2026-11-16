-module(shop_chars).

%% erlc compiles this module. ELP 1.1.0's own parser reports a syntax error (P1711) on every character literal with a
%% hexadecimal escape, as on the tables of jose's base64 modules.

-export([currency_signs/0]).

currency_signs() ->
    {
        $\x{20AC},
        $\x{00A3},
        $\x{00A5},
        $\x{20BD},
        $\x{20B9}
    }.
