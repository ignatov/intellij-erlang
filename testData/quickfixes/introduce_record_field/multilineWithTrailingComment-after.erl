-record(rec, {
  socket,   % the socket
  buffer,    % pending bytes
  new_field
}).

foo() -> #rec{<caret>new_field=100}