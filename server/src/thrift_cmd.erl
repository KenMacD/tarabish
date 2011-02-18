-module(thrift_cmd).

-include("tarabish_thrift.hrl").
-include("tarabish_constants.hrl").

-export([start/0, start/1, stop/1, handle_function/2, getVersion/0,
    createAccount/3, login/2, create_table/0]).

getVersion() ->
  ?tarabish_PROTOCOL_VERSION.

createAccount(Name, Email, Password) ->
  case account:create(Name, Email, Password) of
      {ok, _} -> ok;
      {error, Reason} -> throw(#invalidOperation{why=atom_to_list(Reason)});
      _ -> throw(#invalidOperation{why="Unknown"})
  end.

login(Name, Password) ->
  login(Name, Password, get(client)).

login(Name, Password, undefined) ->
  case account:validate(Name, Password) of
    {ok, Id} ->
      {ok, Client, Cookie} = tarabish_server:get_client(Id),
      put(client, Client),
      Cookie;
    {error, Reason} -> throw(#invalidOperation{why=atom_to_list(Reason)});
    _ -> throw(#invalidOperation{why="Unknown"})
  end;

login(_Name, _Password, _) ->
  throw(#invalidOperation{why="Already Authenticated"}).
 
create_table() ->
  create_table(get(client)).

create_table(undefined) ->
  throw(#invalidOperation{why="Need login first"});

create_table(Client) ->
  {ok, TableId} = client:create_table(Client),
  TableId.

start() ->
  start(42745).

start(Port) ->
  Handler = ?MODULE,
  {ok, Pid} = thrift_socket_server:start([{handler, Handler},
%  thrift_socket_server:start([{handler, Handler},
                              {service, tarabish_thrift},
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
