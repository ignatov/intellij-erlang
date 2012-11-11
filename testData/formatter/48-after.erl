-export([x/1, y/2, z/1, w/1]).

-spec f/2 :: (atom(),
    atom()) -> term().

f(A, B) when is_list(A), is_list(B);
  is_binary(A), is_binary(B) ->
  f().

f(A, B)
  when is_list(A), is_list(B);
  is_binary(A), is_binary(B) ->
  f().