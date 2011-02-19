-module(tarabish_server).

-behaviour(gen_server).

-export([start/0, get_client/1, get_client_by_cookie/1, create_table/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

% id is Id -> {Client, Cookie}
% cookie is Cookie -> Id
% tables is orddict of table_id -> table
% table_cnt counter for next id
-record(state, {id, cookie, tables, table_cnt}).

% Public:
start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

get_client(Id) ->
  gen_server:call({global, ?MODULE}, {get_client, Id}).

get_client_by_cookie(Cookie) ->
  gen_server:call({global, ?MODULE}, {get_client_by_cookie, Cookie}).

create_table(Client) ->
  gen_server:call({global, ?MODULE}, {create_table, Client}).

% gen_server:
init([]) ->
  {ok, #state{id=orddict:new(),
              cookie=orddict:new(),
              tables = [],
              table_cnt = 0}}.

% TODO: Set monitor on client, clear cookie when dies.
handle_call({get_client, Id}, _From, State) ->
  case orddict:find(Id, State#state.id) of
    {ok, {Client, Cookie}} ->
      {reply, {ok, Client, Cookie}, State};
    error -> 
      {ok, Client} = client:start(Id),
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

handle_call({create_table, Client}, _From, State) ->
  NextId = State#state.table_cnt + 1,
  {ok, NewTable} = table:start(NextId, Client),
  Tables1 = [NewTable|State#state.tables],
  {reply, {ok, NewTable, NextId}, State#state{tables=Tables1, table_cnt=NextId}};

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

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

% Internal functions
new_cookie() ->
  <<Cookie:64>> = crypto:rand_bytes(8),
  Cookie.
