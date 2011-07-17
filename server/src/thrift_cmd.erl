-module(thrift_cmd).

-include("tarabish_thrift.hrl").
-include("tarabish_constants.hrl").

-export([start/0, start/1, stop/1, handle_function/2]).

% From Thrift
-export([getVersion/0, createAccount/3, login/2, chat/2,
    join_table/1, get_tables/0, sit/2, start_game/1, call_trump/2]).

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

chat(TableId, Message) ->
  chat(get(client), TableId,  Message).

chat(undefined, _TableId, _Message) ->
  throw(#invalidOperation{why="Need login first"});

chat(Client, TableId, Message) ->
  case client:send_chat(Client, TableId, Message) of
    ok ->
      ok;
    {error, Reason} ->
      throw(#invalidOperation{why=atom_to_list(Reason)})
  end.

join_table(TableId) ->
  join_table(get(client), TableId).

join_table(undefined, _TableId) ->
  throw(#invalidOperation{why="Need login first"});

join_table(Client, TableId) ->
  case client:join_table(Client, TableId) of
    ok ->
      ok;
    {error, Reason} ->
      throw(#invalidOperation{why=atom_to_list(Reason)})
  end.

get_tables() ->
  get_tables(get(client)).

get_tables(undefined) ->
  throw(#invalidOperation{why="Need login first"});

get_tables(_Client) ->
  case tarabish_server:get_tables() of
    {ok, Tables} ->
      Tables;
    {error, Reason} ->
      throw(#invalidOperation{why=atom_to_list(Reason)})
  end.

sit(TableId, Seat) ->
  sit(TableId, Seat, get(client)).

sit(_TableId, _Seat, undefined) ->
  throw(#invalidOperation{why="Need login first"});

sit(TableId, Seat, Client) ->
  case client:sit(Client, TableId, Seat) of
    ok ->
      ok;
    {error, Reason} ->
      throw(#invalidOperation{why=atom_to_list(Reason)})
  end.

start_game(TableId) ->
  start_game(TableId, get(client)).

start_game(_TableId, undefined) ->
  throw(#invalidOperation{why="Need login first"});

start_game(TableId, Client) ->
  case client:start_game(Client, TableId) of
    ok ->
      ok;
    {error, Reason} ->
      throw(#invalidOperation{why=atom_to_list(Reason)})
  end.

call_trump(TableId, Suit) ->
  call_trump(TableId, Suit, get(client)).

call_trump(_TableId, _Suit, undefined) ->
  throw(#invalidOperation{why="Need login first"});

call_trump(TableId, Suit, Client) ->
  case client:call_trump(Client, TableId, Suit) of
    ok ->
      ok;
    {error, Reason} ->
      throw(#invalidOperation{why=atom_to_list(Reason)})
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

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
  case apply(?MODULE, Function, tuple_to_list(Args)) of
    ok -> ok;
    Reply -> {reply, Reply}
  end.
