-export([f/1]).

-define(FIELDS, field1, field2 = default_value).
-define(ELEMENT_BASE(Module), is_element=is_element, module=Module, id, anchor, actions, show_if=true, class="", style="", html_id="").
-record(my_record, {?FIELDS, field3}).
-record(h1, {?ELEMENT_BASE(element_h1), text="", html_encode=true}).

-define(AV_BASE(Module,Type), is_action=Type, module=Module, anchor, trigger, target, actions, show_if=true).
-define(ACTION_BASE(Module), ?AV_BASE(Module,is_action)).
-record(actionbase, {?ACTION_BASE(undefined)}).

f(#my_record{}=A) ->
  B = A#my_record.field3,
  C = A#my_record.field2,
  D = A#my_record.field1,
  foo(B, C, D),
  #h1{id=ola},
  #actionbase{target=ola}.

foo(A, B, C) -> A + B + C.