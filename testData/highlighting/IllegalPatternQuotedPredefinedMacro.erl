-export([f/1, g/1, h/1, i/1, j/1, ok/1]).

f(<error>?'MODULE' ++ R</error>) -> R.
g(<error>?'LINE' ++ R</error>) -> R.
h(<error>?'MACHINE' ++ R</error>) -> R.
i(<error>[?'MODULE'] ++ R</error>) -> R.
j(<error>[?'FILE'] ++ R</error>) -> R.

ok(?'MODULE_STRING' ++ R) -> R;
ok(?'FILE' ++ R) -> R;
ok([?'LINE'] ++ R) -> R.
