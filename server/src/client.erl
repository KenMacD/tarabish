-module(client).

-include("tarabish_constants.hrl").

-behaviour(gen_server).

-export([start/1, start_link/2, quit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

% From Cmd Socket:
-export([chat/3, join_table/2, part_table/2, sit/3, stand/2,
    start_game/2, call_trump/3, play_card/3, play_bella/2,
    call_run/2, show_run/2]).

% From Msg Socket:
-export([get_events/2, subscribe/2]).

% From Game:
-export([recv_event/2]).

-record(state, {id, tables, events, subscriber, cmdRef}).

% Public:
start(Id) ->
  gen_server:start(?MODULE, [Id, none], []).

start_link(Id, CmdPid) ->
  gen_server:start_link(?MODULE, [Id, CmdPid], []).

quit(Client) ->
  gen_server:call(Client, {stop}).

% gets a cast when an event is added to empty list.
subscribe(Client, Pid) ->
  gen_server:cast(Client, {subscribe, Pid}).

chat(Client, TableId, Message) ->
  gen_server:call(Client, {chat, TableId, Message}).

join_table(Client, TableId) ->
  gen_server:call(Client, {join, TableId}).

part_table(Client, TableId) ->
  gen_server:call(Client, {part, TableId}).

sit(Client, TableId, Seat) ->
  gen_server:call(Client, {sit, TableId, Seat}).

stand(Client, TableId) ->
  gen_server:call(Client, {stand, TableId}).

start_game(Client, TableId) ->
  gen_server:call(Client, {start_game, TableId}).

call_trump(Client, TableId, Suit) ->
  gen_server:call(Client, {call_trump, TableId, Suit}).

play_card(Client, TableId, Card) ->
  gen_server:call(Client, {play_card, TableId, Card}).

play_bella(Client, TableId) ->
  gen_server:call(Client, {play_bella, TableId}).

call_run(Client, TableId) ->
  gen_server:call(Client, {call_run, TableId}).

show_run(Client, TableId) ->
  gen_server:call(Client, {show_run, TableId}).

recv_event(Client, Event) ->
  gen_server:cast(Client, {event, Event}).

get_events(Client, 0) ->
  gen_server:call(Client, {get_events});

% Either of these might fail and throw noproc
get_events(Client, Timeout) ->
  clear_new_event(),
  Events = gen_server:call(Client, {get_events}),
  get_events(Client, Timeout, Events).

get_events(Client, Timeout, []) ->
  Ref = erlang:monitor(process, Client),
  receive
    new_event ->
      erlang:demonitor(Ref),
      clear_possible_down(Ref),
      gen_server:call(Client, {get_events});
    {'DOWN',Ref,process,_,_} ->
      []
  after Timeout ->
      []
  end;

get_events(_Client, _Timeout, Events) ->
  Events.

clear_possible_down(Ref) ->
  receive {'DOWN',Ref,process,_,_} ->
      clear_possible_down(Ref)
  after 0 ->
      ok
  end.

clear_new_event() ->
  receive new_event ->
      clear_new_event()
  after 0 ->
      ok
  end.

init([Id, none]) ->
  {ok, #state{id=Id,
              tables=orddict:new(),
              events=[],
              cmdRef=none}};

init([Id, CmdPid]) ->
  Ref = erlang:monitor(process, CmdPid),
  {ok, #state{id=Id,
              tables=orddict:new(),
              events=[],
              cmdRef=Ref}}.

handle_call({stop}, _From, State) ->
  {stop, normal, ok, State};

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
        {ok, TableView} ->
          % If we were already watching the table is should overwrite
          NewTables = orddict:store(TableId, Table, State#state.tables),
          {reply, {ok, TableView}, State#state{tables=NewTables}};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({stand, TableId}, _From, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:stand(Table, State#state.id) of
        ok ->
          % Stay at table, but as observer
          {reply, ok, State};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end;
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;

handle_call({part, TableId}, _From, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:part(Table, State#state.id) of
        ok ->
          Tables = orddict:erase(TableId, State#state.tables),
          {reply, ok, State#state{tables=Tables}};
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

handle_call({call_trump, TableId, Suit}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      {reply, table:call_trump(Table, State#state.id, Suit), State};
    _ ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({play_card, TableId, Card}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      {reply, table:play_card(Table, State#state.id, Card), State};
    _ ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({play_bella, TableId}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      {reply, table:play_bella(Table, State#state.id), State};
    _ ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({call_run, TableId}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      {reply, table:call_run(Table, State#state.id), State};
    _ ->
      {reply, {error, not_at_table}, State}
  end;

handle_call({show_run, TableId}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      {reply, table:show_run(Table, State#state.id), State};
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

handle_info({'DOWN',Ref,process,_,_}, #state{cmdRef=Ref} = State) ->
  {stop, normal, State};

handle_info(Info, State) ->
  io:format("~w recieved unknown info ~p~n",
    [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, #state{id=Id, tables=Tables} = _State) ->
  orddict:map(fun(_TableId, Table) -> table:part(Table, Id) end, Tables),
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

