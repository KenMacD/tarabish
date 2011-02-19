-module(client).

-include("tarabish_constants.hrl").

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

% From Cmd Socket:
-export([create_table/1, send_chat/3]).

% From Msg Socket:
-export([get_events/1]).

% From Game:
-export([recv_chat/3]).

-record(state, {id, tables, events}).

% Public:
start(Id) ->
  gen_server:start(?MODULE, [Id], []).

create_table(Client) ->
  gen_server:call(Client, {create_table, Client}).

send_chat(Client, TableId, Message) ->
  gen_server:call(Client, {chat, TableId, Message}).

recv_chat(Client, TableId, Message) ->
  gen_server:cast(Client, {event, chat, TableId, Message}).

get_events(Client) ->
  gen_server:call(Client, {get_events}).

init([Id]) ->
  {ok, #state{id=Id,
              tables=orddict:new(),
              events=[]}}.

handle_call({create_table, Client}, _From, State) ->
  {ok, Table, TableId} = tarabish_server:create_table(Client),
  NewTables = orddict:store(TableId, Table, State#state.tables),
  {reply, {ok, TableId}, State#state{tables=NewTables}};

handle_call({chat, TableId, Message}, _From, State) ->
  case orddict:find(TableId, State#state.tables) of
    {ok, Table} ->
      R = table:chat(Table, State#state.id, Message),
      {reply, R, State};
    error ->
      {reply, {error, no_table}, State}
  end;

handle_call({get_events}, _From, State) ->
  Reply = lists:reverse(State#state.events),
  {reply, Reply, State#state{events=[]}};

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({event, chat, TableId, Message}, State) ->
  Event = #event{type=?tarabish_EventType_CHAT,
                 table=TableId,
                 message=Message},
  NewEvents = [Event|State#state.events],
  {noreply, State#state{events=NewEvents}};

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
