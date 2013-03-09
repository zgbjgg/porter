{application, porter,
 [
  {description, "Broadcast UDP"},
  {vsn, "0.1"},
  {registered, []},
  {modules,[porter_app, porter_sup, porter]},
  {applications, [kernel,stdlib]},
  {mod, { porter_app, []}},
  {env, [{port, 2070}, {connect, <<"connect:">>}, {disconnect, <<"disconnect:">>}]}]}.
