t(X) ->
?assertMatch({ok, Y} when Y > 1,
foo(X)),
?assertMatch({ok, Y}
when Y > 1,
X),
ok.
