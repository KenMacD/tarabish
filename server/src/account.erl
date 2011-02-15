-module(account).

-export([install/2]).
-export([create/3, validate/2]).

-include_lib("eunit/include/eunit.hrl").

-record(account, {
    user,
    email,
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

create(User, Email, Password)
    when is_binary(User),
         is_binary(Email),
         is_binary(Password) ->
  F = fun() ->
    case mnesia:read(account, User) of
      [_] ->
        {error, account_exists};
      _ ->
        Account = #account {
          user = User,
          email = Email,
          password = erlang:md5(Password)
        },
        ok = mnesia:write(Account),
        {ok, User}
    end
  end,
  {atomic, D} = mnesia:transaction(F),
  D.

validate(User, Password)
    when is_binary(User),
         is_binary(Password) ->
  F = fun() ->
    case mnesia:read(account, User) of
      [P] ->
        case P#account.password =:= erlang:md5(Password) of
          true ->
            {ok, User};
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
      ?_assertEqual({ok, <<"Alice">>}, create(<<"Alice">>, <<"a@invalid">>, <<"Pass">>)),
      ?_assertEqual({ok, <<"Bob">>}, create(<<"Bob">>, <<"b@invalid">>, <<"OtherPass">>)),
      ?_assertEqual({error, account_exists}, create(<<"Alice">>, <<"a@invalid">>, <<"Pass">>)),
      ?_assertEqual({error, account_exists}, create(<<"Alice">>, <<"a@invalid">>, <<"Wrong">>))
    ]}.

verfiy_test_() ->
  {setup, local,
    fun () ->
        setup(),
        {ok, _} = create(<<"Alice">>, <<"a@invalid">>, <<"Pass">>)
    end,
    fun cleanup/1,
    fun (_) ->
        [
          ?_assertEqual({ok, <<"Alice">>}, validate(<<"Alice">>, <<"Pass">>)),
          ?_assertEqual({error, invalid}, validate(<<"Alice">>, <<"Wrong">>)),
          ?_assertEqual({error, invalid}, validate(<<"Bob">>, <<"Any">>))
        ]
    end}.

