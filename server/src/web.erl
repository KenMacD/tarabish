-module(web).

-export([start/0]).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

start() ->
  Port = 42745,
  ok = application:start(ranch),
  ok = application:start(cowboy),

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

% Test of example code.
init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  erlang:start_timer(1000, self(), <<"Hello!">>),
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  {reply, {text, << "You said: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(5000, self(), <<".">>),
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.
