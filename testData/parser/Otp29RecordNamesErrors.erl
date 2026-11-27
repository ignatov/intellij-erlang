-module(otp29_record_names_errors).

index() -> #when.a.
period(R) -> R#Foo.
-type t() :: #Foo{}.
