-export([main/0]).

main() -> ok.
<warning descr="Unused function 'f1/0'">f1</warning>() -> f2().
<warning descr="Unused function 'f2/0'">f2</warning>() -> f1().
