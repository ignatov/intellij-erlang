-record(rec, {old_field = {1, 2}}).

foo() -> #rec{<caret>new_field=100}