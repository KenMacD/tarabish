-module(thrift_cmd).

-include("tarabish_thrift.hrl").
-include("tarabish_constants.hrl").

-export([start/0, start/1, stop/1, handle_function/2]).

% From Thrift
-export([getVersion/0, createAccount/3, login/2]).

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

local(Function, Args) ->
  apply(?MODULE, Function, tuple_to_list(Args)).

server_call(Function, Args) ->
  server_call(Function, Args, get(client)).

server_call(_Function, _Args, undefined) ->
  throw(#invalidOperation{why="Need login first"});

server_call(Function, Args, _Client) ->
  case apply(tarabish_server, Function, tuple_to_list(Args)) of
      ok -> ok;
      {ok, Result} -> Result;
      {error, Reason} ->
        throw(#invalidOperation{why=atom_to_list(Reason)})
    end.

client_call(Function, Args) ->
  client_call(Function, Args, get(client)).

client_call(_Function, _Args, undefined) ->
  throw(#invalidOperation{why="Need login first"});

client_call(Function, Args, Client) ->
  case apply(client, Function, [Client | tuple_to_list(Args)]) of
      ok -> ok;
      {ok, Result} -> Result;
      {error, Reason} ->
        throw(#invalidOperation{why=atom_to_list(Reason)})
    end.

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
  FunctionHandlers =
    [{[getVersion, createAccount, login],
        fun local/2},
     {[get_tables],
        fun server_call/2},
     {[chat, join_table, sit, start_game, call_trump],
        fun client_call/2}],
  case handle_function(Function, Args, FunctionHandlers) of
    ok -> ok;
    Reply -> {reply, Reply}
  end.

handle_function(Function, Args, [{Functions, Handler}|Rest]) ->
  case contains(Function, Functions) of
    true  -> Handler(Function, Args);
    false -> handle_function(Function, Args, Rest)
  end.

contains(_Key, []) ->
  false;
contains(Key, List) ->
  lists:any(fun(X) -> Key == X end, List).
