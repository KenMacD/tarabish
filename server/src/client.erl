-module(client).

-include("tarabish_constants.hrl").

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

% From Cmd Socket:
-export([send_chat/3, join_table/2, sit/3, start_game/2]).

% From Msg Socket:
-export([get_events/2, subscribe/2]).

% From Game:
-export([recv_event/2]).

-record(state, {id, tables, events, subscriber}).

% Public:
start(Id) ->
  gen_server:start(?MODULE, [Id], []).

% gets a cast when an event is added to empty list.
subscribe(Client, Pid) ->
  gen_server:cast(Client, {subscribe, Pid}).

send_chat(Client, TableId, Message) ->
  gen_server:call(Client, {chat, TableId, Message}).

join_table(Client, TableId) ->
  gen_server:call(Client, {join, TableId}).

sit(Client, TableId, Seat) ->
  gen_server:call(Client, {sit, TableId, Seat}).

start_game(Client, TableId) ->
  gen_server:call(Client, {start_game, TableId}).

recv_event(Client, Event) ->
  gen_server:cast(Client, {event, Event}).

get_events(Client, 0) ->
  gen_server:call(Client, {get_events});

% TODO: finish
get_events(Client, Timeout) ->
  clear_new_event(),
  Events = gen_server:call(Client, {get_events}),
  get_events(Client, Timeout, Events).

get_events(Client, Timeout, []) ->
  receive new_event ->
      gen_server:call(Client, {get_events})
  after Timeout ->
      []
  end;

get_events(_Client, _Timeout, Events) ->
  Events.

clear_new_event() ->
  receive new_event ->
      clear_new_event()
  after 0 ->
      ok
  end.

init([Id]) ->
  {ok, #state{id=Id,
              tables=orddict:new(),
              events=[]}}.

handle_call({chat, TableId, Message}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      R = table:chat(Table, State#state.id, Message),
      {reply, R, State};
    error ->
      {reply, {error, no_table}, State}
  end;

% TODO: any risk of deadlock?
handle_call({join, TableId}, _From, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:join(Table, State#state.id, self()) of
        ok ->
          NewTables = orddict:store(TableId, Table, State#state.tables),
          {reply, ok, State#state{tables=NewTables}};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;


handle_call({sit, TableId, Seat}, _From, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:sit(Table, State#state.id, self(), Seat) of
        ok ->
          % If we were already watching the table is should overwrite
          NewTables = orddict:store(TableId, Table, State#state.tables),
          {reply, ok, State#state{tables=NewTables}};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({start_game, TableId}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      {reply, table:start_game(Table, State#state.id), State};
    _ ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({get_events}, _From, State) ->
  Reply = lists:reverse(State#state.events),
  {reply, Reply, State#state{events=[]}};

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({subscribe, Pid}, State) ->
  {noreply, State#state{subscriber=Pid}};

handle_cast({event, Event}, State) ->
  State1 = add_event(Event, State),
  {noreply, State1};

handle_cast(Msg, State) ->
  io:format("~w received unknown cast ~p~n",
    [?MODULE, Msg]),
  {stop, "Bad Cast", State}.

handle_info(Info, State) ->
  io:format("~w recieved unknown info ~p~n",
    [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

new_event(Subscriber) when is_pid(Subscriber) ->
  Subscriber ! new_event;

new_event(_) ->
  ok.

add_event(Event, State = #state{events=[]}) ->
  Subscriber = State#state.subscriber,
  new_event(Subscriber),
  State#state{events=[Event]};

add_event(Event, State) ->
  State#state{events=[Event|State#state.events]}.

