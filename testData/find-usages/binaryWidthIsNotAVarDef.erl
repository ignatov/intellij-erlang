%% Copyright 2019 Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>, Sergey Ignatov
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(binaryWidthIsNotAVarDef).

-export([bwidthNotVar/2]).

bwidthNotVar(Var<caret>, Var2) ->
  %% Variable used to the right of Binary Width in a binary pattern cannot be a new variable declaration,
  %% so Var should have 2 usages: below in binary width and below in X+Var and not just 1 usage
  <<X:Var, _/binary>> = Var2,
  X + Var.
