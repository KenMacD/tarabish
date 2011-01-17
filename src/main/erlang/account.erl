-module(account).

-export([install/2]).
-export([create/2, validate/2]).

-include_lib("eunit/include/eunit.hrl").

-record(account, {
    id,
    user,
    password
  }).

install(RamNodes, DiskNodes)
  when is_list(RamNodes),
       is_list(DiskNodes) ->
         {atomic, ok} = mnesia:create_table(account, [
             {type, set},
             {attributes, record_info(fields, account)},
             {ram_copies, RamNodes},
             {disc_copies, DiskNodes}]).

create(Id, Password) ->
  F = fun() ->
    case mnesia:read(account, Id) of
      [_] ->
        {error, account_exists};
      _ ->
        Account = #account {
          id = Id,
          password = erlang:md5(Password)
        },
        ok = mnesia:write(Account),
        {ok, Id}
    end
  end,
  {atomic, D} = mnesia:transaction(F),
  D.

validate(Id, Password) ->
  F = fun() ->
    case mnesia:read(account, Id) of
      [P] ->
        case P#account.password =:= erlang:md5(Password) of
          true ->
            {ok, Id};
          _ ->
            {error, invalid}
        end;
      _ ->
        {error, invalid}
    end
  end,
  {atomic, D} = mnesia:transaction(F),
  D.

%%% Tests

setup() ->
  cleanup_db(),
  setup_db().

cleanup(_) ->
  cleanup_db().

setup_db() ->
  mnesia:start(),
  install([node()], []),
  ok.

cleanup_db() ->
  case mnesia:system_info(is_running) of
    yes ->
      error_logger:tty(false),
      mnesia:stop(),
      error_logger:tty(true);
    _ -> pass
  end,
  ok.

account_test_() ->
  {setup, local, fun setup/0, fun cleanup/1,
    [
      ?_assertEqual({ok, "Alice"}, create("Alice", "Pass")),
      ?_assertEqual({ok, "Bob"}, create("Bob", "OtherPass")),
      ?_assertEqual({error, account_exists}, create("Alice", "Pass")),
      ?_assertEqual({error, account_exists}, create("Alice", "Wrong"))
    ]}.

verfiy_test_() ->
  {setup, local,
    fun () ->
        setup(),
        {ok, _} = create("Alice", "Pass")
    end,
    fun cleanup/1,
    fun (_) ->
        [
          ?_assertEqual({ok, "Alice"}, validate("Alice", "Pass")),
          ?_assertEqual({error, invalid}, validate("Alice", "Wrong")),
          ?_assertEqual({error, invalid}, validate("Bob", "Any"))
        ]
    end}.

