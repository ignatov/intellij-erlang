%%  Copyright 2012-2014 Sergey Ignatov
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.

-include_lib("testapp-1/include/recursiveLib.hrl").

-export([foo1/0, foo2/0, foo3/0, foo4/0, foo5/0, foo6/0, foo7/0]).

% function resolve

foo1() ->
  resolved_function().

foo2() ->
  <warning>unresolved_function</warning>().

% macro resolve

foo3() ->
  ?resolved_macro.

foo4() ->
  <error>?unresolved_macro</error>.

% record resolve

foo5() ->
  #resolved_record{resolved_field=10}.

foo6() ->
  #resolved_record{<error>unresolved_field</error>=10}.

foo7() ->
  #<error>unresolved_record</error>{unresolved_field=10}.