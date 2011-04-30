-module(game).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("tarabish_constants.hrl").
-include("tarabish_types.hrl").

-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1, determine_dealer/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {table, score1, score2, deck, dealer}).

%% ====================================================================
%% External functions
%% ====================================================================
start(TablePid) ->
  gen_server:start(?MODULE, [TablePid], []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
% TODO: monitor table
init([Table]) ->

  % TODO: use crypto:rand_bytes instead of random for shuffle
  % seed random number generator
  {A1,A2,A3} = now(),
  random:seed(A1, A2, A3),

  % in 0, 1, 2, 3
  Dealer = determine_dealer(Table, deck:shuffle(deck:new())),

  DealerEvent = #event{type=?tarabish_EventType_DEALER, seat=Dealer},
  table:broadcast(Table, DealerEvent),

  Deck = deck:shuffle(deck:new()),

  {ok, state_name, #state{table=Table,
                          score1=0,
                          score2=0,
                          deck=Deck,
                          dealer=Dealer}}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
state_name(Event, StateData) ->
    {next_state, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(Event, From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
determine_dealer(Table, Deck) ->
  Dealer = determine_dealer(Table, Deck, [0,1,2,3]),
  Dealer.

determine_dealer(Table, Deck, [Player|[]]) ->
  Player;

determine_dealer(Table, Deck, Players) when is_list(Players) ->
  {Cards, Rest} = lists:split(length(Players), Deck),
  deal_one(Table, Deck, Players),
  HighCards = deck:high_card(Cards),
  % O(n^2) for n = 4
  PlayerMapper = fun(PlayerNum) -> lists:nth(PlayerNum + 1, Players) end,
  Players1 = lists:map(PlayerMapper, HighCards),
  determine_dealer(Table, Rest, Players1).

deal_one(Table, Deck, []) ->
  Deck;
deal_one(Table, Deck, [Player|Others]) ->
  [Card|Rest] = Deck,
  %table:deal_one_up(Table, Player, Card),
  deal_one(Table, Rest, Others).

%% --------------------------------------------------------------------
%%% Tests
%% --------------------------------------------------------------------

-define(J, #card{value=?tarabish_JACK}).
-define(N, #card{value=9}).
-define(A, #card{value=?tarabish_ACE}).
-define(T, #card{value=10}).
-define(E, #card{value=8}).

determine_dealer_test_() ->
  [
    ?_assertEqual(0, determine_dealer(self(), [?J, ?N, ?N, ?N])),
    ?_assertEqual(1, determine_dealer(self(), [?J, ?J, ?N, ?N, ?T, ?A])),
    ?_assertEqual(3, determine_dealer(self(), [?J, ?J, ?J, ?J,
                                               ?N, ?N, ?N, ?N,
                                               ?A, ?A, ?A, ?A,
                                               ?E, ?E, ?E, ?T]))
  ].

