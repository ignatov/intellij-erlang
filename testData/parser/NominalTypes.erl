-module(nominal_types).

%% Nominal type definitions (EEP 69, OTP 28)
-nominal meter() :: integer().
-nominal foot() :: integer().
-nominal celsius() :: float().

%% Nominal type with parameters
-nominal pair(A, B) :: {A, B}.

%% Parenthesized attribute form
-nominal(quoted() :: atom()).

-export_type([meter/0, foot/0, pair/2]).

%% Nominal types usable alongside regular and opaque types
-type regular() :: meter() | foot().
-opaque hidden() :: celsius().

-spec convert(meter()) -> foot().
convert(M) ->
    M * 3.

-spec make_pair(A, B) -> pair(A, B).
make_pair(A, B) ->
    {A, B}.
