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
    call_run/2, show_run/2, get_tables/1]).

% From Msg Socket:
-export([get_events/3, subscribe/2]).

% From Game:
-export([recv_event/2]).

-record(state, {id,
                tables,
                events=[],
                subscriber,
                cmdRef,
                eventCnt=1,
                timer=none,
                timerRef}).

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

get_tables(Client) ->
  gen_server:cast(Client, {get_tables, self()}).

join_table(Client, TableId) ->
  gen_server:call(Client, {join, TableId}).

part_table(Client, TableId) ->
  gen_server:call(Client, {part, TableId}).

sit(Client, TableId, Seat) ->
  gen_server:cast(Client, {sit, TableId, Seat}).

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

get_events(Client, LastEventNum, 0) ->
  gen_server:call(Client, {get_events, LastEventNum});

% Either of these might fail and throw noproc
get_events(Client, LastEventNum, Timeout) ->
  clear_new_event(),
  client:subscribe(Client, self()),
  Events = gen_server:call(Client, {get_events, LastEventNum}),
  get_events(Client, LastEventNum, Timeout, Events).

get_events(Client, LastEventNum, Timeout, []) ->
  Ref = erlang:monitor(process, Client),
  receive
    new_event ->
      erlang:demonitor(Ref),
      clear_possible_down(Ref),
      gen_server:call(Client, {get_events, LastEventNum});
    {'DOWN',Ref,process,_,_} ->
      []
  after Timeout ->
      erlang:demonitor(Ref),
      []
  end;

get_events(_Client, _LastEventNum, _Timeout, Events) ->
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
              cmdRef=none}};

init([Id, CmdPid]) ->
  Ref = erlang:monitor(process, CmdPid),
  {ok, #state{id=Id,
              tables=orddict:new(),
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

handle_call({get_events, LastEventNum}, _From, State) ->
  State1 = clear_events_timer(State, LastEventNum),
  Reply = lists:reverse(State1#state.events),
  {reply, Reply, State1};

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({get_tables, From}, State) ->
  {ok, Tables} = tarabish_server:get_tables(),
  TablePrint = io_lib:format("Table List: ~p~n", [Tables]),
  web:send_tables(From, TablePrint),
  {noreply, State};

handle_cast({sit, TableId, Seat}, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:sit(Table, State#state.id, self(), Seat) of
        ok ->
          % If we were already watching the table is should overwrite
          NewTables = orddict:store(TableId, Table, State#state.tables),
          {noreply, State#state{tables=NewTables}};
        {error, Reason} ->
          % TODO: some error
          {noreply, State}
      end;
    {error, Reason} ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({subscribe, Pid}, State) ->
  {noreply, State#state{subscriber=Pid}};

handle_cast({event, Event}, State) ->
  State1 = set_timer(State),
  State2 = add_event(Event, State1),
  {noreply, State2};

handle_cast(Msg, State) ->
  io:format("~w received unknown cast ~p~n",
    [?MODULE, Msg]),
  {stop, "Bad Cast", State}.

% If this is the current timer then quit.
handle_info({ping, Ref}, #state{timerRef=Ref} = State) ->
  {stop, normal, State};

handle_info({ping, _Ref}, State) ->
  {noreply, State};

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

set_timer(#state{timer=none} = State) ->
  Ref = make_ref(),
  Timer = erlang:send_after(60000, self(), {ping, Ref}),
  State#state{timer=Timer, timerRef=Ref};

set_timer(State) ->
  State.

% Check eventCnt == LastEventNum and do nothing?
clear_events_timer(#state{events=Events} = State, LastEventNum) ->
  LastSplitter = fun(E) -> E#event.number > LastEventNum end,
  {New, Before} = lists:splitwith(LastSplitter, Events),
  State1 = clear_timer(State, Before),
  State1#state{events=New}.

clear_timer(#state{timer=none} = State, _Events) ->
  State;

% No events cleared, so don't clear the timer:
clear_timer(#state{} = State, []) ->
  State;

clear_timer(#state{timer=Timer} = State, _Events) ->
  erlang:cancel_timer(Timer),
  State#state{timer=none}.

% TODO: handle 32bit overflow on eventCnt?
add_event(Event, State = #state{events=[], eventCnt=Count}) ->
  Event1 = Event#event{number = Count},
  Subscriber = State#state.subscriber,
  new_event(Subscriber),
  State#state{events=[Event1], eventCnt=Count+1};

add_event(Event, #state{eventCnt=Count} = State) ->
  Event1 = Event#event{number = Count},
  State#state{events=[Event1|State#state.events], eventCnt=Count+1}.

