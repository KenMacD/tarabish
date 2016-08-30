-module(system_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
  [test1].

test1(_Config) ->
  % Setup Environment (might move to setup)
  mnesia:stop(),
  mnesia:start(),
  crypto:start(),
  account:install([node()], []),
  {ok, _TarabishServer} = tarabish_server:start_link(),

  % Create one table
  {ok, _TablePid, TableId} = tarabish_server:create_table(),

  % Create 4 clients:
  {ok, Alice} = client:login("Alice"),
  {ok, Bob}   = client:login("Bob"),
  {ok, Carol} = client:login("Carol"),
  {ok, Dave}  = client:login("Dave"),

  % TODO: test join/part when observe works
  % TODO: test start_game not seated

  % All sit, Alice and Bob try to take same seat
  ok = client:sit(Alice, TableId, 0),
  ok = client:sit(Bob,   TableId, 1),
  ok = client:sit(Carol, TableId, 2),
  ok = client:sit(Dave,  TableId, 3),

  % Alice and Bob change seats:
  ok = client:part_table(Alice, TableId),
  ok = client:part_table(Bob, TableId),
  ok = client:sit(Alice, TableId, 1),
  ok = client:sit(Bob, TableId, 0),

  % Can now start the game:
  ok = client:start_game(Alice, TableId),

  % TODO: these are now casts, test they actually work.

  ok.
