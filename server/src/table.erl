-module(table).

-behaviour(gen_server).

-export([start/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([chat/3, join/2]).

-record(person, {client}).
-record(state, {id, owner, members}).

% Public:
start(Id, Client) ->
  gen_server:start(?MODULE, [Id, Client], []).

%% TODO: check if table is still alive
chat(Table, From, Message) ->
  gen_server:cast(Table, {chat, From, Message}).

join(Table, Client) ->
  gen_server:call(Table, {join, Client}).

% gen_server:

init([Id, Client]) ->
  Person = #person{client=Client},
  {ok, #state{id=Id, owner=Person, members=[Person]}}.

handle_call({join, Client}, _From, State) ->
  case is_member(Client, State#state.members) of
    false ->
      NewMembers = [#person{client=Client}|State#state.members],
      {reply, ok, State#state{members=NewMembers}};
    true ->
      {reply, {error, already_joined}, State}
  end;

handle_call(Request, _From, State) ->
  io:format("~w received unknown call ~p~n",
    [?MODULE, Request]),
  {stop, "Bad Call", State}.

handle_cast({chat, From, Message}, State) ->
  ExpandedMessage = bjoin([From, <<" --> ">>, Message]),
  send_chat(State#state.id, ExpandedMessage, State#state.members),
  {noreply, State};

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

% private:
is_member(Client, Members) ->
  lists:keysearch(Client, #person.client, Members).

send_chat(_TableId, _Message, []) ->
  ok;

send_chat(TableId, Message, [Person|Others]) ->
  Client = Person#person.client,
  client:recv_chat(Client, TableId, Message),
  send_chat(TableId, Message, Others).

bjoin(List) ->
  F = fun(A, B) -> <<A/binary, B/binary>> end,
  lists:foldr(F, <<>>, List).

