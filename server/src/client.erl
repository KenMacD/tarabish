-module(client).

-include("tarabish_constants.hrl").

-behaviour(gen_server).

-export([login/1, quit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

% From Web Socket commands:
-export([chat/3, join_table/2, part_table/2, sit/3, stand/2,
    start_game/2, call_trump/3, play_card/3, play_bella/2,
    call_run/2, show_run/2, get_tables/1]).

% From Game:
-export([recv_event/2]).

-record(state, {id, tables, socket}).

% Public:
login(Id) ->
  gen_server:start_link(?MODULE, [Id, self()], []).

quit(Client) ->
  gen_server:call(Client, {stop}).

chat(Client, TableId, Message) ->
  gen_server:cast(Client, {chat, TableId, Message}).

get_tables(Client) ->
  gen_server:cast(Client, {get_tables, self()}).

join_table(Client, TableId) ->
  gen_server:call(Client, {join, TableId}).

part_table(Client, TableId) ->
  gen_server:cast(Client, {part, TableId}).

sit(Client, TableId, Seat) ->
  gen_server:cast(Client, {sit, TableId, Seat}).

stand(Client, TableId) ->
  gen_server:cast(Client, {stand, TableId}).

start_game(Client, TableId) ->
  gen_server:cast(Client, {start_game, TableId}).

call_trump(Client, TableId, Suit) ->
  gen_server:cast(Client, {call_trump, TableId, Suit}).

play_card(Client, TableId, Card) ->
  gen_server:cast(Client, {play_card, TableId, Card}).

play_bella(Client, TableId) ->
  gen_server:cast(Client, {play_bella, TableId}).

call_run(Client, TableId) ->
  gen_server:cast(Client, {call_run, TableId}).

show_run(Client, TableId) ->
  gen_server:cast(Client, {show_run, TableId}).

recv_event(Client, Event) ->
  gen_server:cast(Client, {event, Event}).

init([Id, Socket]) ->
  case tarabish_server:login(Id) of
    {ok} ->
      io:format("Valid login for ~p~n", [Id]),
      web:set_client(Socket, self(), Id),
      {ok, #state{id=Id, tables=orddict:new(), socket=Socket}};
    {error, Reason} ->
      io:format("Failed login for ~p: ~p~n", [Id, Reason]),
      {error, Reason}
  end.

handle_call({stop}, _From, State) ->
  {stop, normal, ok, State};

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

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({get_tables, From}, State) ->
  {ok, Tables} = tarabish_server:get_tables(),
  web:send_tables(From, Tables),
  {noreply, State};

handle_cast({chat, TableId, Message}, State) ->
  io:format("Received chat ~p~n", [Message]),
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      % TODO: R could be error?
      R = table:chat(Table, State#state.id, Message),
      {noreply, State};
    error ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({play_card, TableId, Card}, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      % TODO: handle return
      table:play_card(Table, Card),
      {noreply, State};
    _ ->
      % TODO: handle error
      {noreply, State}
  end;

handle_cast({sit, TableId, Seat}, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:sit(Table, State#state.id, Seat) of
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

handle_cast({part, TableId}, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:part(Table) of
        ok ->
          Tables = orddict:erase(TableId, State#state.tables),
          {noreply, State#state{tables=Tables}};
        {error, Reason} ->
          % TODO: some error
          {noreply, State}
      end;
    {error, Reason} ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({start_game, TableId}, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      % TODO: handle return?
      table:start_game(Table),
      {noreply, State};
    _ ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({call_trump, TableId, Suit}, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      % TODO: handle return
      table:call_trump(Table, Suit),
      {noreply, State};
    _ ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({call_run, TableId}, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      % TODO: handle return?
      table:call_run(Table),
      {noreply, State};
    _ ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({show_run, TableId}, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      % TODO: handle return?
      table:show_run(Table),
      {noreply, State};
    _ ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({play_bella, TableId}, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      % TODO: handle return?
      table:play_bella(Table),
      {noreply, State};
    _ ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({stand, TableId}, State) ->
  case tarabish_server:get_table(TableId) of
    {ok, Table} ->
      case table:stand(Table) of
        ok ->
          % Stay at table, but as observer
          {noreply, State};
        {error, Reason} ->
          % TODO: some error
          {noreply, State}
      end;
    {error, Reason} ->
      % TODO: some error
      {noreply, State}
  end;

handle_cast({event, Event}, #state{socket=Socket} = State) ->
  web:send_event(Socket, Event),
  {noreply, State};

handle_cast(Msg, State) ->
  io:format("~w received unknown cast ~p~n",
    [?MODULE, Msg]),
  {stop, "Bad Cast", State}.

handle_info({'DOWN',Ref,process,_,_}, #state{socket=Ref} = State) ->
  {stop, normal, State};

handle_info(Info, State) ->
  io:format("~w recieved unknown info ~p~n",
    [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, #state{tables=Tables} = _State) ->
  orddict:map(fun(_TableId, Table) -> table:part(Table) end, Tables),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
