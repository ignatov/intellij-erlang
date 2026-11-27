-export([f/0, g/0, acc/1, pat/1, upd/1, acc2/1, pat2/1, upd2/1]).
-record('when', {a}).
-record('Foo', {b}).

%% OTP 29: a record name after # may be a reserved word or look like a variable
f() -> #when{a = 1}.
g() -> #Foo{b = 2}.
acc(R) -> R#when.a.
pat(#when{a = A}) -> A.
upd(R) -> R#when{a = 2}.
acc2(R) -> R#Foo.b.
pat2(#Foo{b = B}) -> B.
upd2(R) -> R#Foo{b = 3}.
