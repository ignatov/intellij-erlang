-module(preview).
-record(config, {port}).

connect(Host, Port, Options) -> {Host, Port, Options}.

start(#config{port = 0}) -> error;
start(#config{}) -> ok.

-spec rate(Level :: low | high) -> integer().
rate(low) -> 1;
rate(high) -> 10.

demo(Options, Config) ->
    connect(/*<# Host: #>*/"localhost", /*<# Port: #>*/8080, Options),
    rate(/*<# Level: #>*/high),
    /*<# block opt:start=erlang.parameter.hints.pattern.summaries #>*/
    start(/*<# #config{}: #>*/Config).
    /*<# block opt:end #>*/
