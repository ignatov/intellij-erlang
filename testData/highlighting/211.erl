-module(<error>examplemodel</error>, [Id, IntParam::integer(), BoolParam::boolean(), JustAParam]).
-compile(export_all).

validation_tests() ->
[
   {fun() -> Id > 0 and BoolParam and JustAParam end, "IntParam must be positive!"}
].