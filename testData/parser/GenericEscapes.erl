-module('GenericEscapes').
-export([chars/0, atoms/0, string/0]).

chars() -> [$\:, $\/, $\a, $\y, $\(, $\#, $\8, $\9, $\é].

atoms() -> ['\:', 'a\/b', '\8', '\é'].

string() -> "\:\/".
