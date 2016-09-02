-module(tarabish_server).

-behaviour(gen_server).

%% Public:
-export([start_link/0, get_client/1, create_table/0,
    get_table/1, get_tables/0, get_client_if_new/2, get_client_if_new/1]).

-export([login/1]).

%% From tables
-export([update_table_image/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

% id is Id -> Client
% tables is orddict of table_id -> table
% table_cnt counter for next id
-record(state, {id, tables, tables_view, table_cnt}).

% Public:
start_link() ->
  io:format(" [*] Tarbish Server Started~n"),
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

get_client(Id) ->
  gen_server:call({global, ?MODULE}, {get_client, Id}).

login(Name) ->
  gen_server:call({global, ?MODULE}, {login, Name, self()}).

get_client_if_new(Id) ->
  gen_server:call({global, ?MODULE}, {get_new_client, Id}).

get_client_if_new(Id, CmdPid) ->
  gen_server:call({global, ?MODULE}, {get_new_client, Id, CmdPid}).

create_table() ->
  gen_server:call({global, ?MODULE}, {create_table}).

get_table(TableId) ->
  gen_server:call({global, ?MODULE}, {get_table, TableId}).

get_tables() ->
  gen_server:call({global, ?MODULE}, {get_tables}).

% From Tables:
update_table_image(TableId, TableView) ->
  gen_server:cast({global, ?MODULE}, {update_table, TableId, TableView}).

% gen_server:
init([]) ->
  process_flag(trap_exit, true),

  % Create some starting tables:
  {ok, NewTable1} = table:start(1),
  {ok, NewTable2} = table:start(2),
  {ok, NewTable3} = table:start(3),
  Tables = orddict:from_list(
	     [{1, NewTable1},
	      {2, NewTable2},
	      {3, NewTable3}]),
  {ok, #state{id=orddict:new(),
              tables=Tables,
              tables_view=orddict:new(),
              table_cnt = 3}}.

handle_call({create_table}, _From, State) ->
  NextId = State#state.table_cnt + 1,
  {ok, NewTable} = table:start(NextId),
  Tables1 = orddict:store(NextId, NewTable, State#state.tables),
  {reply, {ok, NewTable, NextId}, State#state{tables=Tables1,
                                              table_cnt=NextId}};

handle_call({get_table, TableId}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} -> {reply, {ok, Table}, State};
    error -> {reply, {error, invalid}, State}
  end;

handle_call({get_tables}, _From, State) ->
  Tables = tables_view_to_list(State#state.tables_view),
  {reply, {ok, Tables}, State};

handle_call({login, Id, Client}, _From, State) ->
  case orddict:find(Id, State#state.id) of
    {ok, _Client} ->
      {reply, {error, inuse}, State};
    error ->
      monitor(process, Client),
      NewId = orddict:store(Id, Client, State#state.id),
      % TODO: monitor client, remove when it goes down.
      {reply, {ok}, State#state{id=NewId}}
  end;

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({update_table, TableId, TableView}, State) ->
  TablesView1 = orddict:store(TableId, TableView, State#state.tables_view),
  {noreply, State#state{tables_view=TablesView1}};

handle_cast(Msg, State) ->
  io:format("~w received unknown cast ~p~n",
    [?MODULE, Msg]),
  {stop, "Bad Cast", State}.

match_client(Client, Client) ->
  true;

match_client(_Client, _Anything) ->
  false.

not_client(Client) -> fun(_K, V) -> not match_client(Client, V) end.

handle_info({'DOWN', _MonitorRef, process, Client, _Info}, State) ->
  NewId = orddict:filter(not_client(Client), State#state.id),
  {noreply, State#state{id=NewId}};

handle_info(Info, State) ->
  io:format("~w recieved unknown info ~p~n",
    [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions
tables_view_to_list(TableViews) ->
  {_TableIds, Views} = lists:unzip(orddict:to_list(TableViews)),
  Views.
