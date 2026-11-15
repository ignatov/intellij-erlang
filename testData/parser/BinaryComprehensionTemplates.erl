-module(binary_comprehension_templates).

var(B) -> << B || _ <- [1] >>.

case_template(L) -> << case X of 1 -> <<1>>; _ -> <<0>> end || X <- L >>.

call_in_parens(L) -> << (integer_to_binary(X)) || X <- L >>.

binary_template(L) -> << <<X:8>> || X <- L >>.

nested(L) -> << << Y || Y <- X >> || X <- L >>.

macro_template(L) -> << ?BIN || _ <- L >>.

begin_template(L) -> << begin <<X>> end || X <- L >>.
