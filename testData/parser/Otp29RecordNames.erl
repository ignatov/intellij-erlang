-module(otp29_record_names).
-record('when', {a}).
-record('Foo', {b}).

-type t() :: #m:'Foo'{} | #m:Foo{}.

new() -> {#when{a = 1}, #Foo{b = 2}, #'Foo'{b = 3}, #case{}, #andalso{}, #Foo{}}.
get(R) -> {R#when.a, R#Foo.b, R#Foo.'b', (R#Foo{b = 1})#Foo.b}.
match(#when{a = A}, #Foo{b = B}) when A =:= B -> ok.
update(R) -> R#when{a = 2}#Foo{b = 3}.
nested() -> #Foo{b = #when{a = 1}}.
qualified(R) -> {#m:when{a = 1}, #m:Foo{}, R#m:Foo.b, R#m:Foo{b = 1}}.
spaced(R) -> {R# Foo.b, R# when.a, R#%c
Foo.b, R#
  when.a}.
-define(X, #when{}).
-define(Y(Rec), #Foo{b = Rec}).
