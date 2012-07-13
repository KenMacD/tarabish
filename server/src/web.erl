-module(web).

-include("tarabish_thrift.hrl").
-include("tarabish_constants.hrl").

-export([start/0]).

-export([init/3, handle/2, terminate/2]).

-record(state, {processor, transport}).

start() ->
  Port = 42747,
  application:start(sockjs),
  application:start(cowboy),

  SockjsState = sockjs_handler:init_state(
    <<"/test">>, fun service_test/3, #state{}, []),
  VhostRoutes = [
    {[<<"test">>, '...'], sockjs_cowboy_handler, SockjsState},
    {[<<"static">>, '...'], cowboy_http_static, [{directory, "docroot"}]},
    {'_', ?MODULE, []}],
  Routes = [{'_', VhostRoutes}], % any vhost

  io:format(" [*] Running at http://localhost:~p~n", [Port]),

  cowboy:start_listener(http, 100,
    cowboy_tcp_transport, [{port,     Port}],
    cowboy_http_protocol, [{dispatch, Routes}]),

  receive
    _ -> ok
  end.

init({_Any, http}, Req, []) ->
  {ok, Req, []}.

handle(Req, State) ->
  {ok, Data} = file:read_file("./docroot/index.html"),
  {ok, Req1} = cowboy_http_req:reply(200, [{<<"Content-Type">>, "text/html"}],
                                     Data, Req),
  {ok, Req1, State}.

terminate(_Req, _State) ->
  ok.

service_test(Conn, init, State) ->
%   Conn:send(sockjs_json:encode({struct:q
  {ok, #state{}};

service_test(_Conn, {recv, Data}, #state{transport = Transport} = State) ->
  case sockjs_json:decode(Data) of
    {ok, V} -> handle_json(V);
    {error, E} -> ok
  end,
  {ok, State};

service_test(_Conn, closed, State) ->
  {ok, state}.

handle_json(Object) ->
  io:format(" [*] Received Data ~p~n", [Object]).
