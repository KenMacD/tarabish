-module(web).

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

handle_request(Req, "/lobby/") ->
  io:format("Start of lobby~n"),
  % TODO: check rest?
  {Cookie, _Rest} = string:to_integer(get_parameter("session", Req:parse_cookie())),
  io:format("Cookie: ~w~n", [Cookie]),
  case tarabish_server:get_client_by_cookie(Cookie) of
    {ok, _Client} ->
      io:format("FOUND CLIENT!~n");
    {error, _Reason} ->
      io:format("No Client :(~n")
  end;

handle_request(Req, "/login/") ->
  io:format("Start of login~n"),
  Name = get_parameter("name", Req:parse_qs()),
  io:format("Name is ~w~n", [Name]),
  case tarabish_server:get_client_if_new(Name) of
    {ok, _Client, Cookie} ->
      SessCookie = mochiweb_cookies:cookie("session", Cookie, [{path, "/"}]),
      io:format("Call received: Cookie: ~w~n", [Cookie]),
      Req:respond({302, [SessCookie, {"Location", "/lobby/"}], <<>>});
    error ->
      Req:ok({"text/html;charset=UTF-8", "INVALID"})
  end.

loop(Req) ->
  catch case Req:get(version) of
    Version when Version >= {1, 0} ->
      Path = Req:get(path),
      handle_request(Req, Path);
    _ -> ok
  end.
