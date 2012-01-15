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

handle_request(Req, "/cmd/") ->
  io:format("Start of cmd~n"),
  Data = Req:recv_body(),
  OutData = thrift_process(Data),
  io:format("OutData: ~s~n", [OutData]),
  Req:ok({"text/json", [], OutData});

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
  io:format("Other Path ~s~n", [OtherPath]),
  case lists:prefix("/static/", OtherPath) of
    true ->
      io:format("Loading file from ~s~n", [string:substr(OtherPath,
            string:len("/static/"))]),
      Req:serve_file(string:substr(OtherPath, string:len("/static/ ")), "docroot");
    false ->
      case get_client(Req) of
        {ok, Client} ->
          io:format("Have client~n"),
          handle_request(Req, OtherPath, Client);
        {error, _Reason} ->
          io:format("No client, need login~n"),
          Req:respond({302, [{"Location", "/"}], <<>>})
      end
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

thrift_process(Data) ->
  InProtoGen = fun() ->
      {ok, MemoryTransport} = thrift_memory_buffer:new(Data),
      {ok, JP} = thrift_json_protocol:new(MemoryTransport),
      JP
  end,
  OutProtoGen = fun() ->
      {ok, MemoryTransport2} = thrift_memory_buffer:new(),
      {ok, JP2} = thrift_json_protocol:new(MemoryTransport2),
      JP2
  end,
  {OutProto, ok} = thrift_processor:init({self(),
                                          InProtoGen,
                                          OutProtoGen,
                                          tarabish_thrift,
                                          thrift_cmd}),
  {_OutProto2, {ok, ReplyTransport}} = thrift_protocol:get_transport(OutProto),
  {_ReplyTransport2, {ok, ReplyData}} = thrift_transport:read(ReplyTransport, 1024 * 1024),
  ReplyData.

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
