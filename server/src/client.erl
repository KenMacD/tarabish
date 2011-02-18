-module(client).

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([create_table/1]).

-record(state, {id, tables}).

% Public:
start(Id) ->
  gen_server:start(?MODULE, [Id], []).

create_table(Client) ->
  gen_server:call(Client, {create_table}).

% gen_server:

init([Id]) ->
  {ok, #state{id=Id,
              tables=orddict:new()}}.

handle_call({create_table}, _From, State) ->
  {ok, Table, TableId} = tarabish_server:create_table(),
  NewTables = orddict:store(TableId, Table, State#state.tables),
  {reply, {ok, TableId}, State#state{tables=NewTables}};

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
