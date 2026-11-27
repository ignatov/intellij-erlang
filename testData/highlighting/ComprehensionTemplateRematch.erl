-export([m/1, l/1, g/1]).

m(_ = N) -> #{case ok of {<<_:N>>} -> ok end => try ok catch error:N -> ok end || ok}.

l(_ = N) -> [{case ok of {<<_:N>>} -> ok end, try ok catch error:N -> ok end} || ok].

g(L) -> #{K => begin V = K, V end || K <- L}.
