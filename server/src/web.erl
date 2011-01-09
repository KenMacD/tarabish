-module(web).

-include("tarabish_thrift.hrl").
-include("tarabish_constants.hrl").

-export([start/0]).

-export([loop/1]).

start() -> 
  io:format("Starting mochiweb~n"),
  {ok, WebPID} = mochiweb_http:start([{port, 42747}, {loop, {?MODULE, loop}}]),
  unlink(WebPID),
  {ok, WebPID}.

get_parameter(N, [{K,V}|_]) when K == N -> V;
get_parameter(N, [_|T]) -> get_parameter(N, T);
get_parameter(_, _) -> [].

get_client(Req) ->
  % TODO: check rest?
  {Cookie, _Rest} = string:to_integer(get_parameter("session", Req:parse_cookie())),
  tarabish_server:get_client_by_cookie(Cookie).

handle_request(Req, "/getVersion/") ->
  VersionEncoded = json_encode(tarabish_thrift:function_info('getVersion',
      reply_type), ?tarabish_PROTOCOL_VERSION),
  Req:ok({"text/json", [], VersionEncoded});

handle_request(Req, "/login/") ->
  io:format("Start of login~n"),
  Name = get_parameter("name", Req:parse_post()),
  io:format("Name is ~w~n", [Name]),
  case tarabish_server:get_client_if_new(Name) of
    {ok, _Client, Cookie} ->
      SessCookie = mochiweb_cookies:cookie("session", Cookie, [{path, "/"}]),
      io:format("Call received: Cookie: ~w~n", [Cookie]),
      Req:respond({302, [SessCookie, {"Location", "/lobby/"}], <<>>});
    error ->
      Req:ok({"text/html;charset=UTF-8", "INVALID"})
  end;

handle_request(Req, "/") ->
  io:format("Start of index~n"),
  Req:serve_file("index.html", "docroot");

handle_request(Req, OtherPath) ->
  io:format("Start of OtherPath~n"),
  case get_client(Req) of
    {ok, Client} ->
      io:format("Have client"),
      handle_request(Req, OtherPath, Client);
    {error, _Reason} ->
      io:format("No client, need login"),
      Req:respond({302, [{"Location", "/"}], <<>>})
  end.

handle_request(Req, "/lobby/", _Client) ->
  io:format("Start of lobby~n"),
  Req:serve_file("lobby.html", "docroot");

handle_request(Req, "/lobby/refresh/", _Client) ->
  {ok, TableList} = tarabish_server:get_tables(),
  TableListEncoded = json_encode(tarabish_thrift:function_info('getTables',
      reply_type), TableList),

  Req:ok({"text/json", [], TableListEncoded}).

loop(Req) ->
  catch case Req:get(version) of
    Version when Version >= {1, 0} ->
      Path = Req:get(path),
      handle_request(Req, Path);
    _ -> ok
  end.

% 
% Type: tarabish_thrift:function_info('getTables', reply_type).
% Data
json_encode(Type, Data) ->
  {ok, MemoryTransport} = thrift_memory_buffer:new(),
  {ok, JsonProtocolIn} = thrift_json_protocol:new(MemoryTransport),
  {Protocol, ok} = thrift_protocol:write(JsonProtocolIn, {Type, Data}),
  {protocol, thrift_json_protocol, JsonProtocol} = Protocol,
  {json_protocol, Transport, _, _} = JsonProtocol,
  {transport, thrift_memory_buffer, MemoryBuffer} = Transport,
  {memory_buffer, OutData} = MemoryBuffer,

  iolist_to_binary(OutData).
