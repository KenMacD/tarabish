-module(web).

-export([start/0]).
-behaviour(cowboy_websocket_handler).

-export([set_client/3]).

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
  ets:insert(webcmd, {<<"login">>, tarabish_server, login, [name]}),

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

% Section 3 - websocket server

% Test of example code.
init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, Req, #state{}}.

websocket_handle({text, Msg}, Req, State) ->
  Data = jsx:decode(Msg, [{labels, existing_atom}]),

  io:format("Message: ~p~n", [Data]),
  Method = proplists:get_value(method, Data),
  io:format("Method: ~p~n", [Method]),
  % TODO: Send error event if this fails:
  handle_method(ets:lookup(webcmd, Method), Data),
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({event, Event}, Req, State) ->
  {reply, {text, Event}, Req, State};

% TODO prevent double login
websocket_info({client, Client, Cookie}, Req, State) ->
  % TODO: link to it?
  % TODO: send event
  L = io_lib:format("You are logged in. Cookie ~p~n", [Cookie]),
  {reply, {text, L}, Req, State#state{client=Client, cookie=Cookie}};

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(5000, self(), <<".">>),
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

handle_method([{Binary, Mod, Fun, Params}], Data) ->
  handle_method(Mod, Fun, [], lists:reverse(Params), Data);

handle_method([], Data) ->
  ok.

handle_method(Mod, Fun, Params, [], Data) ->
  io:format("Calling ~p ~p with ~p~n", [Mod, Fun, Params]),
  apply(Mod, Fun, Params);

handle_method(Mod, Fun, Params, [P|Rest], Data) ->
  Value = proplists:get_value(P, Data),
  case Value of
    undefined -> ok;
    _ -> handle_method(Mod, Fun, [Value|Params], Rest, Data)
  end.
