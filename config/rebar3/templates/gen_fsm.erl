-module({{name}}_fsm).
-behaviour(gen_fsm).

-export(
  [ start_link/0

  %Callbacks
  , init/1
  , terminate/3
  , code_change/4
  % , StateName/2
  % , StateName/3
  , handle_sync_event/4
  , handle_event/3
  , handle_info/3
  ]). %Callbacks

-record(state, {}).
-type state() :: #state{}.

-type async_return() :: {next_state, StateName::atom(), state()} |
                        {next_state, StateName::atom(), state(), Timeout::pos_integer()} |
                        {next_state, StateName::atom(), state(), hibernate} |
                        {stop, Reason::term(), state()}.

-type sync_return() :: {reply, Reply::term(), StateName::atom(), state()} |
                       {reply, Reply::term(), StateName::atom(), state(), Timeout::pos_integer()} |
                       {reply, Reply::term(), StateName::atom(), state(), hibernate} |
                       async_return().

% ----- Public -----
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

% ----- Private -----

% ----- Callbacks -----
-spec init(term()) ->
  {ok, StateName::atom(), state()} |
  {ok, StateName::atom(), state(), Timeout::pos_integer()} |
  {ok, state(), hibernate}.
init([]) ->
  StateName = init_state,
  {ok, StateName, #state{}}.

-spec terminate(normal|shutdown|{shutdown,term()}|term(), StateName::atom(), state()) -> term().
terminate(_Reason, _StateName, _StateData) ->
  ok.

-spec code_change(term()|{down, term()}, StateName::atom(), state(), term()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

% -spec StateName(Event::term(), From::{pid(),term()}, state()) -> sync_return().
% StateName(_Event, _From, StateName, StateData) ->
%   {reply, ok, State}.
%
% -spec StateName(Event::term(), state()) -> async_return().
% StateName(_Event, StateName, StateData) ->
%   {next_state, StateName, StateData}.

-spec handle_sync_event(Event::term(), {pid(),term()}, StateName::atom(), state()) -> sync_return().
handle_sync_event(_Event, _From, StateName, StateData) ->
  {reply, ok, StateName, StateData}.

-spec handle_event(Event::term(), StateName::atom(), state()) -> async_return().
handle_event(_Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

-spec handle_info(timeout | term(), StateName::atom(), state()) -> async_return().
handle_info(_Info, StateName, StateData) ->
  {next_state, StateName, StateData}.
