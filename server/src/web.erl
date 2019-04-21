-module(web).

-export([start/0]).
-behaviour(cowboy_websocket).

-export([set_client/3, send_tables/2, send_event/2]).

-record(state, {client}).
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

% Section 1 - Cowboy Setup
start() ->
  Port = 42745,

  % New API:
  ets:new(webcmd, [named_table, {read_concurrency, true}]),

  % Funcation, Mod, Params, SendClient?
  ets:insert(webcmd, {login, client, [name], false}),
  ets:insert(webcmd, {get_tables, client, [], true}),

  ets:insert(webcmd, {sit, client, [table_id, seat], true}),
  ets:insert(webcmd, {part_table, client, [table_id], true}),
  ets:insert(webcmd, {chat, client, [table_id, message], true}),
  ets:insert(webcmd, {start_game, client, [table_id], true}),
  ets:insert(webcmd, {call_trump, client, [table_id, suit], true}),
  ets:insert(webcmd, {play_card, client, [table_id, card], true}),
  ets:insert(webcmd, {call_run, client, [table_id], true}),
  ets:insert(webcmd, {show_run, client, [table_id], true}),
  ets:insert(webcmd, {play_bella, client, [table_id], true}),

  % TODO: setup as application as use priv_dir
  %{ok, Cwd} = file:get_cwd(),
  Dispatch = cowboy_router:compile([
      {'_', [
        {"/", cowboy_static, {priv_file, tarabish, "docroot/index.html"}},
        {"/websocket", ?MODULE, []},
        {"/[...]", cowboy_static, {priv_dir, tarabish, "docroot"}}
      ]}]),

  {ok, _} = cowboy:start_clear(web, [{port, Port}],
    #{env => #{dispatch => Dispatch}}),

  io:format(" [*] Running at http://localhost:~p~n", [Port]).


% Section 2 - API


set_client(Server, Client, Id) ->
  Server ! {client, Client, Id}.

send_tables(Server, Tables) ->
  Server ! {tables, Tables}.

send_event(Server, Event) ->
  Server ! {event, Event}.

% Section 3 - websocket server

% Test of example code.
init(Req, _Opts) ->
  {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, State}.

websocket_handle({text, Msg}, #state{client=Client} = State) ->
  Data = jsx:decode(Msg, [{labels, existing_atom}]),

  io:format("Message: ~p~n", [Data]),
  Method = proplists:get_value(method, Data),
  io:format("Method: ~p~n", [Method]),
  % TODO: Send error event if this fails:
  try binary_to_existing_atom(Method, utf8) of
    MethodAtom ->
      handle_method(ets:lookup(webcmd, MethodAtom), Data, Client),
      {ok, State}
  catch
    error:badarg -> {ok, State}
  end;

websocket_handle(_Data, State) ->
  {ok, State}.

%websocket_info({event, Event}, Req, State) ->
%  {reply, {text, Event}, Req, State};

websocket_info({event, Event}, State) ->
  try jsx:encode(Event) of
    Eventj -> {reply, {text, Eventj}, State}
  catch
    error:badarg ->
      io:format("Badarg message ~p~n", [Event]),
      {ok, State};
    error:function_clause ->
      io:format("Function clause message ~p~n", [Event]),
      {ok, State}
  end;

% TODO prevent double login
websocket_info({client, Client, Id}, State) ->
  link(Client),
  % TODO: send event

  Event = jsx:encode([
    {type, <<"valid_login">>},
    {name, Id}]),
  %L = io_lib:format("{\You are logged in. Cookie ~p~n", [Cookie]),
  {reply, {text, Event}, State#state{client=Client}};

websocket_info({tables, Tables}, State) ->
  Event = jsx:encode([
      {type, <<"tables">>},
      {tables, Tables}]),
  {reply, {text, Event}, State};

websocket_info({timeout, Msg}, State) ->
  % erlang:start_timer(5000, self(), <<".">>),
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.


terminate(_Reason, _Req, #state{client=undefined}) ->
  io:format("Websocket terminated~n"),
  ok;

terminate(_Reason, _Req, #state{client=Client}) ->
  io:format("Websocket terminated~n"),
  client:quit(Client),
  ok.

% Client method with no client:
handle_method([{_Fun, _Mod, _Params, true}], _Data, undefined) ->
  io:format("Bad call, no client~n"),
  % TODO: return an error
  ok;

handle_method([{Fun, Mod, Params, NeedClient}], Data, Client) ->
  % TODO: handle error
  {ok, Args} = get_args(lists:reverse(Params), Data),
  Args2 = case NeedClient of
    true  -> [Client|Args];
    false -> Args end,
  io:format("Calling ~p ~p with ~p~n", [Mod, Fun, Args2]),
  apply(Mod, Fun, Args2);

handle_method([], _Data, _Client) ->
  ok.

get_args(Params, Data) ->
  get_args([], Params, Data).

get_args(Values, [], _Data) ->
  {ok, Values};

get_args(Values, [P|Rest], Data) ->
  Value = proplists:get_value(P, Data),
  case Value of
    undefined -> {error};
    _ -> get_args([Value|Values], Rest, Data)
  end.
