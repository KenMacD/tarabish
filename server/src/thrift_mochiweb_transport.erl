%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(thrift_mochiweb_transport).

-behaviour(thrift_transport).

-export([new/1]).

% callbacks:
-export([read/2,
         write/2,
         flush/1,
         close/1
        ]).

-record(state, {req, req_data = undefined, data = <<>>, done = false}).

new(Req) ->
    State = #state{req = Req},
    thrift_transport:new(?MODULE, State).

write(#state{done = true} = State, _Data) ->
    {State, {error, done}};

write(#state{data = <<>>} = State, Data) ->
    {State#state{data=[Data]}, ok};

write(#state{data = Content} = State, Data) ->
    {State#state{data=[Data|Content]}, ok}.


read(#state{done = true} = State, _Len) ->
    {State, {error, done}};

read(State, 0) ->
    {State, {ok, <<>>}};

read(#state{req = Req, req_data = undefined} = State, Len) ->
    read(State#state{req_data = Req:recv_body()}, Len);

read(#state{req_data = <<>>} = State, _Len) ->
    {State, {error, 'EOF'}};

read(#state{req_data = Data} = State, Len) ->
    Give = min(size(Data), Len),
    {Result, Remaining} = split_binary(Data, Give),
    {State#state{req_data = Remaining}, {ok, Result}}.


flush(#state{done = true} = State) ->
    {State, {error, done}};

flush(#state{req = Req, data = Data} = State) ->
    ReplyData = iolist_to_binary(lists:reverse(Data)),
    Req:ok({"text/json", ReplyData}),
    {State#state{data = <<>>, done=true}, ok}.

close(#state{} = State) ->
    % ignore
    {State, ok}.
