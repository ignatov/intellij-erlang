-record(rec, {new_field}).

foo() -> #rec{<caret>new_field=100}