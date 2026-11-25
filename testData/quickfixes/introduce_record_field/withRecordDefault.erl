-record(other, {}).
-record(rec, {old_field = #other{}}).

foo() -> #rec{<caret>new_field=100}