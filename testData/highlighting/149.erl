-export([f/1]).

-define(FIELDS, field1, field2 = default_value).
-record(my_record, {?FIELDS, field3}).

f(#my_record{}=A) ->
  B = A#my_record.field3,
  C = A#my_record.field2,
  D = A#my_record.field1,
  foo(B, C, D).
