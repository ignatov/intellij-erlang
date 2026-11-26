-record(rec, {
  socket,   % the socket
  buffer    % pending bytes
}).

foo() -> #rec{<caret>new_field=100}