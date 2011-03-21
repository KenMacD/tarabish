-module(system_SUITE).

-compile(export_all).

-include("ct.hrl").

all() ->
  [test1].

test1(_Config) ->
  % Setup Environment (might move to setup)
  mnesia:stop(),
  mnesia:start(),
  crypto:start(),
  account:install([node()], []),

  {ok, _TarabishServer} = tarabish_server:start(),

  % Create one table
  {ok, _TablePid, TableId} = tarabish_server:create_table(),

  % Create 4 clients:
  {ok, Alice} = client:start("Alice"),
  {ok, Bob}   = client:start("Bob"),
  {ok, Carol} = client:start("Carol"),
  {ok, Dave}  = client:start("Dave"),

  % All join the table
  ok = client:join_table(Alice, TableId),
  ok = client:join_table(Bob,   TableId),
  ok = client:join_table(Carol, TableId),
  ok = client:join_table(Dave,  TableId),

  % Can't start, not sitting:
  {error, _} = client:start_game(Alice, TableId),

  % All sit, Alice and Bob try to take same seat
  ok = client:sit(Alice, TableId, 0),
  {error, _} = client:sit(Bob, TableId, 0),
  ok = client:sit(Bob,   TableId, 1),
  ok = client:sit(Carol, TableId, 2),
  ok = client:sit(Dave,  TableId, 3),

  % Can now start the game:
  ok = client:start_game(Alice, TableId),

  ok.