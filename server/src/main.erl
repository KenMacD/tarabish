-module(main).

-export([start/0]).

start() ->
  mnesia:stop(),
  mnesia:start(),
  install_tables([node()], []), % Install all in RAM for now
  crypto:start(),
  tarabish_server:start(),
  tarabish_server:create_table(),
  tarabish_server:create_table(),
  tarabish_server:create_table(),
  thrift_cmd:start(),
  web:start().

install_tables(RamNodes, DiskNodes)
  when is_list(RamNodes),
       is_list(DiskNodes) ->
  account:install(RamNodes, DiskNodes).
