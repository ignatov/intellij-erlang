-record(test_record, {}).
-record(record1, {foo, bar}).

f1() -> ok.

f2() -> case ok of
          ok -> #test_recor<caret>d;
          _ -> ok
        end.

f3() -> ok.

-record(record2, {foo, bar}).