{application, ti_app,
 [{description, "An application whose gen_server times out right after init/1"},
  {vsn, "0.1.0"},
  {modules, [ti_app, ti_sup, ti_server]},
  {registered, [ti_sup, ti_server]},
  {applications, [kernel, stdlib]},
  {mod, {ti_app, []}}]}.
