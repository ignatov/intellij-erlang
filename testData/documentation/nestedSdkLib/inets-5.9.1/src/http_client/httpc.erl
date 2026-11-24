-module(httpc).
-export([request/1]).
-export_type([request_id/0]).

-type request_id() :: reference().

request(Url) -> Url.
