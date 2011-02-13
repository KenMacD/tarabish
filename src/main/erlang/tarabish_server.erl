-module(tarabish_server).

-behaviour(gen_server).

-export([start/0, get_client/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-record(state, {clients}).

% Public:
start() ->
  gen_server:start({global, ?MODULE}, ?MODULE, [], []).

get_client(Id) ->
  gen_server:call({global, ?MODULE}, {get_client, Id}).

% gen_server:
init([]) ->
  {ok, #state{clients=orddict:new()}}.

handle_call({get_client, Id}, _From, State) ->
  case orddict:find(Id, State#state.clients) of
    {ok, Client} -> {reply, {ok, Client}, State};
    error -> 
      {ok, Client} = client:start(Id),
      NewClients = orddict:store(Id, Client, State#state.clients),
      {reply, {ok, Client}, State#state{clients=NewClients}}
  end; 

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

