-record(rec, {
  a
  % b was here
}).

foo() -> #rec{<caret>new_field=100}