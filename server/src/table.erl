-module(table).

-include("tarabish_types.hrl").

-behaviour(gen_server).

-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).

-export([chat/3, join/3]).

-record(person, {name, client, seat}).
-record(state, {id, members, view}).

% Public:
start(Id) ->
  gen_server:start(?MODULE, [Id], []).

%% TODO: check if table is still alive
chat(Table, From, Message) ->
  gen_server:cast(Table, {chat, From, Message}).

join(Table, ClientName, Client) ->
  gen_server:call(Table, {join, ClientName, Client}).

% gen_server:

init([Id]) ->
  View = new_table_view(Id),
  tarabish_server:update_table_image(Id, View),
  {ok, #state{id=Id, members=[], view=View}}.

handle_call({join, ClientName, Client}, _From, State) ->
  case is_member(Client, State#state.members) of
    false ->
      Person = #person{name=ClientName, client=Client, seat=none},
      NewMembers = [Person|State#state.members],
      % Add observer:
      View = State#state.view,
      Observers = View#tableView.observers,
      View1 = View#tableView{observers=[ClientName|Observers]},
      % Update Server:
      tarabish_server:update_table_image(State#state.id, View1),
      {reply, ok, State#state{members=NewMembers, view=View1}};
    _ExistingPerson ->
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
% by client or clientid?
is_member(Client, Members) ->
  lists:keyfind(Client, #person.client, Members).

send_chat(_TableId, _Message, []) ->
  ok;

send_chat(TableId, Message, [Person|Others]) ->
  Client = Person#person.client,
  client:recv_chat(Client, TableId, Message),
  send_chat(TableId, Message, Others).

bjoin(List) ->
  F = fun(A, B) -> <<A/binary, B/binary>> end,
  lists:foldr(F, <<>>, List).

new_table_view(Id) ->
  #tableView{tableId=Id,
             observers=[]}.
