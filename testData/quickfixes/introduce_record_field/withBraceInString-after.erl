-record(rec, {old_field = "}", new_field}).

foo() -> #rec{<caret>new_field=100}