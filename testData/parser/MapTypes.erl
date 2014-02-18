-type map1() :: #{ binary() => integer() }.
-type map2() :: #{ one | two => [number()] }.

-spec func(Opt, M) -> #{'status' => S, 'c' => integer()} when
  Opt :: 'inc' | 'dec',
  M :: #{'status' => S, 'c' => integer()},
  S :: 'update' | 'keep'.

func(inc, #{status := update, c := C} = M) -> M#{c := C + 1};
func(dec, #{status := update, c := C} = M) -> M#{c := C - 1};
func(_, #{status := keep} = M)             -> M.


-spec plist_to_map(Ls) -> #{ binary() => integer() } when
  Ls :: [{binary(), integer()}].

plist_to_map(PList) ->
  plist_to_map(PList, #{}).

plist_to_map([], M) ->
  M;
plist_to_map([{K,V}|Ls], M) when is_binary(K), is_integer(V) ->
  plist_to_map(Ls, M#{ a => V });
plist_to_map([_|Ls], M) ->
  plist_to_map(Ls, M).