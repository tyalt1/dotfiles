-module({{name}}_statem).
-behaviour(gen_statem).

-export([start_link/0]). %Public API
-export([init/1, terminate/3, code_change/4, callback_mode/0, handle_event/4]). %Callbacks

-record(state, {}).
-type state() :: #state{}.

-type event_type() :: {call, From::{pid(), term()}} | cast | info | timeout | internal.

-type action() :: postpone |
                  {postpone, boolean()} |
                  hibernate |
                  {hibernate, boolean()} |
                  (Timeout::pos_integer()) |
                  {timeout, Timeout::pos_integer(), EventContent::term()} |
                  {reply, From::{pid(), term()}, Reply::term()} |
                  {next_event, EventType::event_type(), EventData::term()}.

% ----- Public -----
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

% ----- Private -----

% ----- Callbacks -----
-spec init(term()) ->
  {ok, StateName::atom(), state()} |
  {ok, StateName::atom(), state(), Actions::[action()] | action()} |
  {stop, Reason::term()}.
init([]) ->
  StateName = init_state,
  {ok, StateName, #state{}}.

-spec terminate(normal|shutdown|{shutdown,term()}|term(), StateName::term(), state()) -> term().
terminate(_Reason, _StateName, _StateData) ->
  ok.

-spec code_change(term()|{down, term()}, StateName::term(), state(), term()) -> {ok, term(), state()}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

-spec callback_mode() -> state_functions | handle_event_function.
callback_mode() ->
  handle_event_function.

-spec handle_event(EventType::event_type(), EventContent::term(), StateName::atom(), state()) ->
  {next_state, StateName::term(), StateData::state()} |
  {next_state, StateName::term(), StateData::state(), Actions::[action()] | action()}.
handle_event(_EventType, _EventContent, StateName, StateData) ->
  {next_state, StateName, StateData}.
