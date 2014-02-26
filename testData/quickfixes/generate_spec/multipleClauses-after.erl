-spec foo(atom(), tuple()) -> atom().

foo(atom, {tuple}) -> atom;
foo(atom1, {tuple, tuple}) -> atom.