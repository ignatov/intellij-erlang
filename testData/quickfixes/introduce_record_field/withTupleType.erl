-record(rec, {old_field :: {atom(), integer()}}).

foo() -> #rec{<caret>new_field=100}