-module({{name}}_statem).
-behaviour(gen_statem).

-export(
  [ start_link/0

  %Callbacks
  , init/1
  , terminate/3
  , code_change/4
  , callback_mode/0
  , handle_event/4
  ]).

-record(data, {}).

-type data() :: #data{}.
-type state() :: atom().

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
start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

% ----- Callbacks -----
-spec init(term()) ->
  {ok, Name::state(), data()} |
  {ok, Name::state(), data(), Actions::[action()] | action()} |
  {stop, Reason::term()}.
init([]) ->
  Name = init_state,
  {ok, Name, #data{}}.

-spec terminate(normal|shutdown|{shutdown,term()}|term(), Name::state(), data()) -> term().
terminate(_Reason, _Name, _Data) ->
  ok.

-spec code_change(term()|{down, term()}, Name::state(), data(), term()) -> {ok, term(), data()}.
code_change(_OldVsn, Name, Data, _Extra) ->
  {ok, Name, Data}.

-spec callback_mode() -> state_functions | handle_event_function.
callback_mode() -> handle_event_function.

-spec handle_event(EventType::event_type(), EventContent::term(), Name::state(), data()) ->
  {next_state, Name::state(), Data::data()} |
  {next_state, Name::state(), Data::data(), Actions::[action()] | action()}.
handle_event(_EventType, _EventContent, Name, Data) ->
  {next_state, Name, Data}.

% ----- Private -----
