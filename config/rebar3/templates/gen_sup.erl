-module({{name}}_sup).
-behaviour(supervisor).

-export(
  [ start_link/0

  % Callback
  , init/1
  ]).

% Helper macro for declaring children of supervisor.
% Example: ?CHILD(module_name, [], worker)
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

% ----- Public -----
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% ----- Callbacks -----
-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  %sup_flags = {strategy, restart_intensity, period}
  %child_spec = {id, {M,F,A}, restart, shutdown, worker | supervisor, [modules]}
  {ok, {{one_for_one, 1, 5}, []}}.
