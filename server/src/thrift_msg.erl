-module(thrift_msg).

-include("tarabishMsg_thrift.hrl").
-include("tarabish_constants.hrl").

-export([start/0, start/1, stop/1, handle_function/2, getVersion/0,
    login/1]).

getVersion() ->
  ?tarabish_PROTOCOL_VERSION.

login(Cookie) ->
  login(Cookie, get(client)).

login(SignedCookie, undefined) when is_integer(SignedCookie) ->
  <<Cookie:64>> = <<SignedCookie:64>>,
  case tarabish_server:get_client_by_cookie(Cookie) of
    {ok, Client} ->
      put(client, Client);
    {error, Reason} -> throw(#invalidOperation{why=atom_to_list(Reason)})
  end;

login(_Cookie, _) ->
  throw(#invalidOperation{why="Already Authenticated"}).

start() ->
  start(42746).

start(Port) ->
  Handler = ?MODULE,
  {ok, Pid} = thrift_socket_server:start([{handler, Handler},
                              {service, tarabishMsg_thrift},
                              {port, Port},
                              {name, ?MODULE},
                              {socket_opts, [{recv_timeout, 60*60*1000}]}]),
  unlink(Pid),
  {ok, Pid}.

stop(Server) ->
  thrift_socket_server:stop(Server).

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
  case apply(?MODULE, Function, tuple_to_list(Args)) of
    ok -> ok;
    Reply -> {reply, Reply}
  end.
