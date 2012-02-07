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
  {Cookie, _Rest} = string:to_integer(get_parameter("clientid", Req:parse_cookie())),
  tarabish_server:get_client_by_cookie(Cookie).

handle_request(Req, "/cmd/") ->
  io:format("Start of cmd~n"),
  {ok, Transport} = thrift_mochiweb_transport:new(Req),
  {ok, Protocol} = thrift_json_protocol:new(Transport),
  thrift_processor:once({Protocol, tarabish_thrift, thrift_cmd});

handle_request(Req, "/login/") ->
  io:format("Start of login~n"),
  Name = get_parameter("name", Req:parse_post()),
  io:format("Name is ~w~n", [Name]),
  case tarabish_server:get_client_if_new(Name) of
    {ok, _Client, Cookie} ->
      SessCookie = mochiweb_cookies:cookie("clientid", Cookie, [{path, "/"}]),
      NameCookie = mochiweb_cookies:cookie("clientname", Name, [{path, "/"}]),
      io:format("Call received: Cookie: ~w~n", [Cookie]),
      Req:respond({302, [SessCookie, NameCookie, {"Location", "/lobby/"}], <<>>});
    error ->
      Req:ok({"text/html;charset=UTF-8", "INVALID"})
  end;

handle_request(Req, "/") ->
  io:format("Start of index~n"),
  Req:serve_file("index.html", "docroot");

handle_request(Req, "/client_id.js") ->
  io:format("Start of client_id~n"),
  Cookies = Req:parse_cookie(),
  {Cookie, _Rest} = string:to_integer(get_parameter("clientid", Cookies)),
  Name = get_parameter("clientname", Cookies),
  Req:ok({"application/javascript",
      lists:concat(["var client_id=", Cookie, ";\n"
                    "var client_name=\"", Name, "\";\n"])});


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

handle_request(Req, "/sit/", Client) ->
  io:format("Start of table~n"),
  QS = Req:parse_qs(),
  TableId = get_parameter("tid", QS),
  {TableIdNum, []} = string:to_integer(TableId),
  SeatId = get_parameter("seat", QS),
  {SeatIdNum, []} = string:to_integer(SeatId),
  ok = client:sit(Client, TableIdNum, SeatIdNum), % TODO: handle error
  Location = ["/table/", TableId, "/", SeatId, "/"],
  Req:respond({302, [{"Location", lists:concat(Location)}], <<>>});

handle_request(Req, Other, Client) ->
  handle_token_request(Req, Client, string:tokens(Other, "/")).

handle_token_request(Req, _Client, ["table", TableId, SeatId]) ->
  io:format("Start of table ~w seat ~w~n", [TableId, SeatId]),
  Req:serve_file("table.html", "docroot").

%handle_request(Req, "/lobby/refresh/", _Client) ->
%  {ok, TableList} = tarabish_server:get_tables(),
%  TableListEncoded = json_encode(tarabish_thrift:function_info('getTables',
%      reply_type), TableList),
%
%  Req:ok({"text/json", [], TableListEncoded}).

loop(Req) ->
  catch case Req:get(version) of
    Version when Version >= {1, 0} ->
      Path = Req:get(path),
      handle_request(Req, Path);
    _ -> ok
  end.