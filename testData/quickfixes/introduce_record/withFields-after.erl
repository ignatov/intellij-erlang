-record(bar, {f1, f2, f3}).

foo() -> #<caret>bar{f1 = "aaa", f2 = 13, f3 = []}
