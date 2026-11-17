-module(sigils).

%% Vanilla sigil (default) - UTF-8 binary
vanilla_sigil() ->
    ~"Hello, World!",
    ~"Björn",
    ~"Line 1\nLine 2".

%% Binary sigil with escape sequences
binary_sigil_lowercase() ->
    ~b"Hello",
    ~b"Tab:\tNewline:\n",
    ~b"Quote: \"test\"".

%% Binary sigil verbatim (no escapes)
binary_sigil_uppercase() ->
    ~B"Hello",
    ~B"No escapes here".

%% String sigil with escape sequences
string_sigil_lowercase() ->
    ~s"Hello",
    ~s"Escape: \n\t",
    ~s"Quote: \"x\"".

%% String sigil verbatim
string_sigil_uppercase() ->
    ~S"Hello",
    ~S"No escape here".

%% Different delimiters - parentheses
paren_delimiters() ->
    ~(hello),
    ~b(binary),
    ~B(verbatim),
    ~s(string),
    ~S(raw string).

%% Different delimiters - brackets
bracket_delimiters() ->
    ~[hello],
    ~b[binary],
    ~B[verbatim],
    ~s[string],
    ~S[raw].

%% Different delimiters - braces
brace_delimiters() ->
    ~{hello},
    ~b{binary},
    ~B{verbatim},
    ~s{string},
    ~S{raw}.

%% Different delimiters - angle brackets
angle_delimiters() ->
    ~<hello>,
    ~b<binary>,
    ~B<verbatim>,
    ~s<string>,
    ~S<raw>.

%% Different delimiters - slash
slash_delimiters() ->
    ~/hello/,
    ~b/binary/,
    ~B/verbatim/,
    ~s/string/,
    ~S/raw/.

%% Different delimiters - pipe
pipe_delimiters() ->
    ~|hello|,
    ~b|binary|,
    ~B|verbatim|,
    ~s|string|,
    ~S|raw|.

%% Different delimiters - single quote
single_quote_delimiters() ->
    ~'hello',
    ~b'binary',
    ~B'verbatim',
    ~s'string',
    ~S'raw'.

%% Different delimiters - backtick
backtick_delimiters() ->
    ~`hello`,
    ~b`binary`,
    ~B`verbatim`,
    ~s`string`,
    ~S`raw`.

%% Different delimiters - hash
hash_delimiters() ->
    ~#hello#,
    ~b#binary#,
    ~B#verbatim#,
    ~s#string#,
    ~S#raw#.

%% Triple-quoted sigils
triple_quoted_sigils() ->
    ~"""
    Multi-line
    verbatim string
    """,
    ~b"""
    Binary with
    escape: \n
    """,
    ~B"""
    Verbatim binary
    no escapes: \n
    """.

%% Sigils in expressions
sigil_expressions() ->
    X = ~"test",
    Y = ~b"binary" ++ ~b"concat",
    io:format(~"Format: ~p~n", [X]),
    case ~"pattern" of
        ~"pattern" -> ok
    end.

%% Sigils with special characters
special_chars() ->
    ~"Unicode: λ α β γ",
    ~b"UTF-8: 日本語".
