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

%% states:
-export([wait_trump/3]).

%% from table:
-export([call_trump/3]).

-record(state, {table,
                hands,    % What the players are holding[[], [], [], []]
                score1,   % Score for player 0, 2
                score2,   % Score for player 1, 3
                deck,     % What's left of the deck
                dealer,   % Which seat is dealing
                trick,    % Trick number (for runs/done)
                toact,    % Player we're waiting on
                toask,    % Players left to ask to act
                order}).  % The deal/play order for this hand

%% ====================================================================
%% External functions
%% ====================================================================
start(TablePid) ->
  gen_fsm:start(?MODULE, [TablePid], []).

call_trump(Game, Seat, Suit) ->
  gen_fsm:sync_send_event(Game, {call_trump, Seat, Suit}).

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

  FirstPlayer = (Dealer + 1) rem 4,
  DealOrder = lists:seq(FirstPlayer, 3) ++ lists:seq(0, FirstPlayer - 1),
  ToAsk = tl(DealOrder),

  State = #state{table=Table,
                 hands=[[], [], [], []],
                 score1=0,
                 score2=0,
                 deck=Deck,
                 dealer=Dealer,
                 order=DealOrder,
                 toact=FirstPlayer,
                 trick=0,
                 toask=ToAsk},
  % State1 = deal3(State),
  % State2 = deal3(State1),

  % TODO-LLL: update deal3 to take state, return state

  % TODO: save cards dealt to players for later verification (on table?)
  % Pass the state and get a new state back
  Deck1 = deal3(Table, Deck,  DealOrder),
  Deck2 = deal3(Table, Deck1, DealOrder),

  State1 = State#state{deck=Deck2},

  AskTrumpEvent = #event{type=?tarabish_EventType_ASK_TRUMP,
                         seat=FirstPlayer},
  table:broadcast(Table, AskTrumpEvent),

  {ok, wait_trump, State1}.

% Handle force the dealer:
wait_trump({call_trump, Seat, ?tarabish_PASS}, _From, #state{toask=[]} = State)
    when State#state.toact =:= Seat ->
  {reply, {error, forced_to_call}, wait_trump, State};

% Other player passes
wait_trump({call_trump, Seat, ?tarabish_PASS = Suit}, _From, State)
    when State#state.toact =:= Seat ->

  Event = #event{type=?tarabish_EventType_CALL_TRUMP, seat=Seat, suit=Suit},
  table:broadcast(State#state.table, Event),

  [NewAct|NewAsk] = State#state.toask,

  AskTrumpEvent = #event{type=?tarabish_EventType_ASK_TRUMP, seat=NewAct},
  table:broadcast(State#state.table, AskTrumpEvent),

  {reply, ok, wait_trump, State#state{toact=NewAct, toask=NewAsk}}; 

% Non pass:
wait_trump({call_trump, Seat, Suit}, _From, State)
    when State#state.toact =:= Seat ->

  Event = #event{type=?tarabish_EventType_CALL_TRUMP, seat=Seat, suit=Suit},
  table:broadcast(State#state.table, Event),
  Deck = deal3(State#state.table, State#state.deck, State#state.order),

  [First|Rest] = State#state.order,

  AskCardEvent = #event{type=?tarabish_EventType_ASK_CARD, seat=First},
  table:broadcast(State#state.table, AskCardEvent),

  % TODO: change to need-card state
  {reply, ok, state_name, State#state{toact=First, toask=Rest, deck=Deck, trick=1}};

wait_trump(_Event, _From, State) ->
  {reply, {error, invalid}, wait_trump, State}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, State) ->
    {next_state, state_name, State}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, _From, State) ->
    {reply, {error, bad_state}, state_name, State}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
determine_dealer(Table, Deck) ->
  Dealer = determine_dealer(Table, Deck, [0,1,2,3]),
  Dealer.

determine_dealer(_Table, _Deck, [Player|[]]) ->
  Player;

determine_dealer(Table, Deck, Players) when is_list(Players) ->
  {Cards, Rest} = lists:split(length(Players), Deck),
  deal_one(Table, Deck, Players),
  HighCards = deck:high_card(Cards),
  % O(n^2) for n = 4
  PlayerMapper = fun(PlayerNum) -> lists:nth(PlayerNum + 1, Players) end,
  Players1 = lists:map(PlayerMapper, HighCards),
  determine_dealer(Table, Rest, Players1).

deal_one(_Table, Deck, []) ->
  Deck;
deal_one(Table, Deck, [_Player|Others]) ->
  [_Card|Rest] = Deck,
  %table:deal_one_up(Table, Player, Card),
  deal_one(Table, Rest, Others).

deal3(_Table, Deck, []) ->
  Deck;

deal3(Table, Deck, [Seat|Others]) ->
  {Cards, Deck1} = lists:split(3, Deck),
  Event = #event{type=?tarabish_EventType_DEAL,
                 seat=Seat,
                 cards=Cards,
                 table=Table},
  table:deal3(Table, Event),
  deal3(Table, Deck1, Others).

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

