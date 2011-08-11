-module(tarabish_server).

-include("tarabish_types.hrl").

-behaviour(gen_server).

%% Public:
-export([start/0, get_client/1, get_client_by_cookie/1, create_table/0,
    get_table/1, get_tables/0, get_client_if_new/1]).

%% From tables
-export([update_table_image/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

% id is Id -> {Client, Cookie}
% cookie is Cookie -> Id
% tables is orddict of table_id -> table
% table_cnt counter for next id
-record(state, {id, cookie, tables, tables_view, table_cnt}).

% Public:
start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

get_client(Id) ->
  gen_server:call({global, ?MODULE}, {get_client, Id}).

get_client_by_cookie(Cookie) ->
  gen_server:call({global, ?MODULE}, {get_client_by_cookie, Cookie}).

get_client_if_new(Id) ->
  gen_server:call({global, ?MODULE}, {get_new_client, Id}).

create_table() ->
  gen_server:call({global, ?MODULE}, {create_table}).

get_table(TableId) ->
  gen_server:call({global, ?MODULE}, {get_table, TableId}).

get_tables() ->
  gen_server:call({global, ?MODULE}, {get_tables}).

% From Tables:
update_table_image(TableId, #tableView{} = TableView) ->
  gen_server:cast({global, ?MODULE}, {update_table, TableId, TableView}).

% gen_server:
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{id=orddict:new(),
              cookie=orddict:new(),
              tables=orddict:new(),
              tables_view=orddict:new(),
              table_cnt = 0}}.

% TODO: remove when using authentication again
handle_call({get_new_client, Id}, _From, State) ->
  case orddict:find(Id, State#state.id) of
    {ok, {_Client, _Cookie}} ->
      {reply, error, State};
    error ->
      {ok, Client} = client:start_link(Id),
      Cookie = new_cookie(),
      NewId = orddict:store(Id, {Client, Cookie}, State#state.id),
      NewCookie = orddict:store(Cookie, Client, State#state.cookie),
      {reply, {ok, Client, Cookie}, State#state{id=NewId, cookie=NewCookie}}
  end;

% TODO: Set monitor on client, clear cookie when dies.
handle_call({get_client, Id}, _From, State) ->
  case orddict:find(Id, State#state.id) of
    {ok, {Client, Cookie}} ->
      {reply, {ok, Client, Cookie}, State};
    error ->
      {ok, Client} = client:start_link(Id),
      Cookie = new_cookie(),
      NewId = orddict:store(Id, {Client, Cookie}, State#state.id),
      NewCookie = orddict:store(Cookie, Client, State#state.cookie),
      {reply, {ok, Client, Cookie}, State#state{id=NewId, cookie=NewCookie}}
  end;

handle_call({get_client_by_cookie, Cookie}, _From, State) ->
  case orddict:find(Cookie, State#state.cookie) of
    {ok, Client} -> {reply, {ok, Client}, State};
    error -> {reply, {error, invalid}, State}
  end;

handle_call({create_table}, _From, State) ->
  NextId = State#state.table_cnt + 1,
  {ok, NewTable} = table:start(NextId),
  Tables1 = orddict:store(NextId, NewTable, State#state.tables),
  % Store new view:
  TablesView1 = orddict:store(NextId,
                              #tableView{tableId=NextId},
                              State#state.tables_view),
  {reply, {ok, NewTable, NextId}, State#state{tables=Tables1,
                                              tables_view=TablesView1,
                                              table_cnt=NextId}};

handle_call({get_table, TableId}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} -> {reply, {ok, Table}, State};
    error -> {reply, {error, invalid}, State}
  end;

handle_call({get_tables}, _From, State) ->
  Tables = tables_view_to_list(State#state.tables_view),
  {reply, {ok, Tables}, State};

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({update_table, TableId, #tableView{} = TableView}, State) ->
  TablesView1 = orddict:store(TableId, TableView, State#state.tables_view),
  {noreply, State#state{tables_view=TablesView1}};

handle_cast(Msg, State) ->
  io:format("~w received unknown cast ~p~n",
    [?MODULE, Msg]),
  {stop, "Bad Cast", State}.

match_client(Client, Client) ->
  true;

match_client(Client, {Client, _Cookie}) ->
  true;

match_client(_Client, _Anything) ->
  false.

not_client(Client) -> fun(_K, V) -> not match_client(Client, V) end.

% TODO: on non-normal exit let all tables know?
handle_info({'EXIT', Client, _Reason}, State) ->
  io:format("Client gone. Before ~w~n", [State#state.id]),
  NewId = orddict:filter(not_client(Client), State#state.id),
  io:format("Client gone. After ~w~n", [NewId]),
  NewCookie = orddict:filter(not_client(Client), State#state.cookie),
  {noreply, State#state{id=NewId, cookie=NewCookie}};

handle_info(Info, State) ->
  io:format("~w recieved unknown info ~p~n",
    [?MODULE, Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal functions
new_cookie() ->
  <<Cookie:64>> = crypto:rand_bytes(8),
  Cookie.

tables_view_to_list(TableViews) ->
  {_TableIds, Views} = lists:unzip(orddict:to_list(TableViews)),
  Views.
