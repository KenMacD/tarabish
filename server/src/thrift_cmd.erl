-module(thrift_cmd).

-include("tarabish_thrift.hrl").
-include("tarabish_constants.hrl").

-export([start/0, start/1, stop/1, handle_function/2]).

% From Thrift
-export([get_version/0, create_account/3, login/2, cap_to_underscore/1]).

get_version() ->
  ?tarabish_PROTOCOL_VERSION.

create_account(Name, Email, Password) ->
  case account:create(Name, Email, Password) of
      {ok, _} -> ok;
      {error, Reason} -> throw(#invalidOperation{why=atom_to_list(Reason)});
      _ -> throw(#invalidOperation{why="Unknown"})
  end.

login(Name, Password) ->
  login(Name, Password, get(client)).

login(Name, _Password, undefined) ->
  case tarabish_server:get_client_if_new(Name, self()) of
    {ok, Client, Cookie} ->
      put(client, Client),
      Cookie;
    error ->
      throw(#invalidOperation{why="Client exists, use cookie"})
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

cap_to_underscore_char(C) when is_integer(C) ->
  Lc = string:to_lower(C),
  case Lc =:= C of
    true  -> C;
    false -> ["_", Lc]
  end.

cap_to_underscore(Function) when is_atom(Function) ->
  FunctionList = [cap_to_underscore_char(C) || C <- atom_to_list(Function)],
  list_to_atom(lists:flatten(FunctionList)).

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
  FunctionNameAtom = cap_to_underscore(Function),
  FunctionHandlers =
    [{[get_version, create_account, login], fun local/2},
     {[get_tables],                       fun server_call/2}],
  case handle_function(FunctionNameAtom, Args, FunctionHandlers) of
    ok -> ok;
    Reply -> {reply, Reply}
  end.

handle_function(Function, Args, []) ->
  client_call(Function, Args);

handle_function(Function, Args, [{Functions, Handler}|Rest]) ->
  case contains(Function, Functions) of
    true  -> Handler(Function, Args);
    false -> handle_function(Function, Args, Rest)
  end.

contains(_Key, []) ->
  false;
contains(Key, List) ->
  lists:any(fun(X) -> Key == X end, List).
