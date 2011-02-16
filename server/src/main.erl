-module(main).

-export([start/0]).

start() ->
  mnesia:stop(),
  mnesia:start(),
  install_tables([node()], []), % Install all in RAM for now
  tarabish_server:start(),
  thrift_cmd:start().

install_tables(RamNodes, DiskNodes)
  when is_list(RamNodes),
       is_list(DiskNodes) ->
  account:install(RamNodes, DiskNodes).
