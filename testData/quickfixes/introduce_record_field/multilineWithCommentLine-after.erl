-record(rec, {
  a,
  % b was here
  new_field
}).

foo() -> #rec{<caret>new_field=100}