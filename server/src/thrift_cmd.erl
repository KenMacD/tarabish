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

login(Name, _Password) ->
  ListName = case is_binary(Name) of
    true  -> binary_to_list(Name);
    false -> Name
  end,
  case tarabish_server:get_client_if_new(ListName) of
    {ok, _Client, Cookie} ->
      Cookie;
    error ->
      throw(#invalidOperation{why="Client exists, use cookie"})
  end.

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
  apply(?MODULE, Function, Args).

server_call(Function, [ClientId|Args]) ->
  server_call(Function, Args, get_client(ClientId)).

server_call(_Function, _Args, undefined) ->
  throw(#invalidOperation{why="Need login first"});

server_call(Function, Args, _Client) ->
  case apply(tarabish_server, Function, Args) of
      ok -> ok;
      {ok, Result} -> Result;
      {error, Reason} ->
        throw(#invalidOperation{why=atom_to_list(Reason)})
    end.

client_call(Function, [ClientId|Args]) ->
  client_call(Function, Args, get_client(ClientId)).

client_call(_Function, _Args, undefined) ->
  throw(#invalidOperation{why="Need login first"});

client_call(Function, Args, Client) ->
  case apply(client, Function, [Client | Args]) of
      ok -> ok;
      {ok, Result} -> Result;
      {error, Reason} ->
        throw(#invalidOperation{why=atom_to_list(Reason)})
    end.

event_call(Function, [ClientId|Args]) ->
  event_call(Function, Args, get_client(ClientId)).

event_call(_Function, _Args, undefined) ->
  throw(#invalidOperation{why="Need login first"});

% For events override missing timeout with zero.
event_call(Function, [], Client) ->
  event_call(Function, [0], Client);

event_call(_Function, [Timeout], Client) ->
  case (catch client:get_events(Client, Timeout)) of
    {'EXIT',{noproc,_Stackdump}} ->
      throw(#invalidOperation{why="Client Gone"}); 
    Other ->
      Other
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
     {[get_tables],                         fun server_call/2},
     {[get_events, get_events_timeout],     fun event_call/2}],
  case handle_function(FunctionNameAtom, Args, FunctionHandlers) of
    ok -> ok;
    Reply -> {reply, Reply}
  end.

handle_function(Function, Args, []) ->
  client_call(Function, tuple_to_list(Args));

handle_function(Function, Args, [{Functions, Handler}|Rest]) ->
  case contains(Function, Functions) of
    true  -> Handler(Function, tuple_to_list(Args));
    false -> handle_function(Function, Args, Rest)
  end.

get_client(SignedCookie) when is_integer(SignedCookie) ->
  <<Cookie:64>> = <<SignedCookie:64>>,
  case tarabish_server:get_client_by_cookie(Cookie) of
    {ok, Client} ->
      Client;
    {error, _Reason} -> % TODO: {ok, _} | {error, Reason} instead of eating it.
      undefined
  end;

get_client(_Other) ->
  undefined.
  
contains(_Key, []) ->
  false;
contains(Key, List) ->
  lists:any(fun(X) -> Key == X end, List).
