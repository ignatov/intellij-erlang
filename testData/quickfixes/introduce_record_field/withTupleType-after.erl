-record(rec, {old_field :: {atom(), integer()}, new_field}).

foo() -> #rec{<caret>new_field=100}