-include("testapp-1/include/testapp.hrl").

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