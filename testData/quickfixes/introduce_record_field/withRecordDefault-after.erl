-record(other, {}).
-record(rec, {old_field = #other{}, new_field}).

foo() -> #rec{<caret>new_field=100}