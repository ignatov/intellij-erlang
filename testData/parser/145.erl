-define(ELEMENT_BASE(Module), is_element=is_element, module=Module, id, anchor, actions, show_if=true, class="", style="", html_id="").
-record(elementbase, {?ELEMENT_BASE(undefined)}).

-record(image, {?ELEMENT_BASE(element_image), image="", alt}).