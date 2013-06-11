-module(web).

-export([start/0]).
-behaviour(cowboy_websocket_handler).

-export([set_client/3, send_tables/2]).

-record(state, {client, cookie}).
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

% Section 1 - Cowboy Setup
start() ->
  Port = 42745,
  ok = application:start(ranch),
  ok = application:start(cowboy),

  % New API:
  ets:new(webcmd, [named_table, {read_concurrency, true}]),

  % Binary, Mod, Function, Params, SendClient?
  ets:insert(webcmd, {<<"login">>, tarabish_server, login, [name], false}),
  ets:insert(webcmd, {<<"get_tables">>, tarabish_server, get_tables, [], false}),

  ets:insert(webcmd, {<<"sit">>, client, sit, [table_id, seat], true}),

  % TODO: setup as application as use priv_dir
  {ok, Cwd} = file:get_cwd(),
  Path = filename:join([Cwd, "docroot"]),
  Dispatch = cowboy_router:compile([
      {'_', [
        {"/", cowboy_static, [
          {directory, Path},
%          {file, "html_ws_client.html"},
          {file, "index.html"},
          {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
        ]},
        {"/websocket", ?MODULE, []},
        {"/static/[...]", cowboy_static, [
          {directory, filename:join([Path, "static"])},
          {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
        ]}
      ]}]),

  {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
    [{env, [{dispatch, Dispatch}]}]),

  io:format(" [*] Running at http://localhost:~p~n", [Port]),

  receive
    _ -> ok
  end.

% Section 2 - API


set_client(Server, Client, Cookie) ->
  Server ! {client, Client, Cookie}.

send_tables(Server, Tables) ->
  Server ! {tables, Tables}.

% Section 3 - websocket server

% Test of example code.
init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, Req, #state{}}.

websocket_handle({text, Msg}, Req, #state{client=Client} = State) ->
  Data = jsx:decode(Msg, [{labels, existing_atom}]),

  io:format("Message: ~p~n", [Data]),
  Method = proplists:get_value(method, Data),
  io:format("Method: ~p~n", [Method]),
  % TODO: Send error event if this fails:
  handle_method(ets:lookup(webcmd, Method), Data, Client),
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({event, Event}, Req, State) ->
  {reply, {text, Event}, Req, State};

% TODO prevent double login
websocket_info({client, Client, Cookie}, Req, State) ->
  link(Client),
  % TODO: send event
  L = io_lib:format("You are logged in. Cookie ~p~n", [Cookie]),
  {reply, {text, L}, Req, State#state{client=Client, cookie=Cookie}};

websocket_info({tables, Tables}, Req, State) ->
  {reply, {text, Tables}, Req, State};

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(5000, self(), <<".">>),
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

% Client method with no client:
handle_method([{_Binary, _Mod, _Fun, _Params, true}], Data, undefined) ->
  io:format("Bad call, no client~n"),
  % TODO: return an error
  ok;

handle_method([{Binary, Mod, Fun, Params, NeedClient}], Data, Client) ->
  % TODO: handle error
  {ok, Args} = get_args(lists:reverse(Params), Data),
  Args2 = case NeedClient of
    true  -> [Client|Args];
    false -> Args end,
  io:format("Calling ~p ~p with ~p~n", [Mod, Fun, Args2]),
  apply(Mod, Fun, Args2);

handle_method([], Data, Client) ->
  ok.

get_args(Params, Data) ->
  get_args([], Params, Data).

get_args(Values, [], Data) ->
  {ok, Values};

get_args(Values, [P|Rest], Data) ->
  Value = proplists:get_value(P, Data),
  case Value of
    undefined -> {error};
    _ -> get_args([Value|Values], Rest, Data)
  end.
