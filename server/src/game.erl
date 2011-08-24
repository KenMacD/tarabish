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
-export([start/1, start_link/1, determine_dealer/2, stop/1]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% states:
-export([wait_trump/3, wait_card/3]).

%% from table:
-export([call_trump/3, play_card/3, call_run/2, show_run/2]).

-record(state, {table,
                score,    % Two scores { , } (0,2 -- 1,3)

                % Set per hand:
                hands,    % What the players are holding{[], [], [], []}
                runs,     % What runs/bella each player has
                runShown, % If a run has been shown
                deck,     % What's left of the deck
                dealer,   % Which seat is dealing
                trick,    % Trick number (for runs/done)
                trump,    % Trump for this hand
                hscore,   % Score for this hand (for bait, etc) { , }
                caller,   % 0 or 1 for who called trump

                % Set per trick:
                order,    % The deal/play order for this hand
                inplay,   % Current cards on the table as [(Card, Seat),]
                htrump,   % Current highest trump in play
                ledin     % Suit that started the thing
              }).

%% ====================================================================
%% External functions
%% ====================================================================
start(TablePid) ->
  gen_fsm:start(?MODULE, [TablePid], []).

start_link(TablePid) ->
  gen_fsm:start_link(?MODULE, [TablePid], []).

stop(GamePid) ->
  gen_fsm:send_all_state_event(GamePid, stop).

call_trump(Game, Seat, Suit) ->
  gen_fsm:sync_send_event(Game, {call_trump, Seat, Suit}).

play_card(Game, Seat, Card) ->
  gen_fsm:sync_send_event(Game, {play_card, Seat, Card}).

call_run(Game, Seat) ->
  gen_fsm:sync_send_event(Game, {call_run, Seat}).

show_run(Game, Seat) ->
  gen_fsm:sync_send_event(Game, {show_run, Seat}).

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

  State = new_hand(Dealer, #state{table=Table, score={0,0}}),

  {ok, wait_trump, State}.


% Handle force the dealer:
wait_trump({call_trump, Seat, ?tarabish_PASS}, _From,
    #state{order = [Seat|[]]} = State) ->
  {reply, {error, forced_to_call}, wait_trump, State};

% Other player passes
wait_trump({call_trump, Seat, ?tarabish_PASS = Suit}, _From,
    #state{order=[Seat|Rest]} = State) ->

  Event = #event{type=?tarabish_EventType_CALL_TRUMP, seat=Seat, suit=Suit},
  table:broadcast(State#state.table, Event),

  AskTrumpEvent = #event{type=?tarabish_EventType_ASK_TRUMP, seat=hd(Rest)},
  table:broadcast(State#state.table, AskTrumpEvent),

  {reply, ok, wait_trump, State#state{order=Rest}};

% Non pass:
wait_trump({call_trump, Seat, Suit}, _From,
    #state{order=[Seat|_Rest]} = State) ->

  Event = #event{type=?tarabish_EventType_CALL_TRUMP, seat=Seat, suit=Suit},
  table:broadcast(State#state.table, Event),
  State1 = deal3(State),
  Runs = runs:new(tuple_to_list(State1#state.hands), Suit),

  State2 = new_trick(State1#state.dealer + 1,
      State1#state{trick=1, trump=Suit, caller=(Seat rem 2), runs=Runs}),

  AskCardEvent = #event{type=?tarabish_EventType_ASK_CARD,
    seat=hd(State2#state.order)},
  table:broadcast(State#state.table, AskCardEvent),

  {reply, ok, wait_card, State2};

wait_trump(_Event, _From, State) ->
  {reply, {error, invalid}, wait_trump, State}.

process_scores([S1, S2] = Scores, State) when S1 > 500, S1 > S2 ->
  Event = #event{type=?tarabish_EventType_GAME_DONE,
                 seat=0,
                 score=Scores},
  table:broadcast(State#state.table, Event),
  {stop, normal, ok, State};

process_scores([S1, S2] = Scores, State) when S2 > 500, S2 > S1 ->
  Event = #event{type=?tarabish_EventType_GAME_DONE,
                 seat=1,
                 score=Scores},
  table:broadcast(State#state.table, Event),
  {stop, normal, ok, State};

process_scores(_Scores, #state{dealer=Dealer} = State) ->
  % no winnder yet
  State1 = new_hand(Dealer + 1, State),
  {reply, ok, wait_trump, State1}.

process_hand(#state{hscore={S1, S2}, caller=Caller} = State) when S1 == S2 ->
  % Half bait
  process_hand(S1 * Caller, S2 * ((Caller + 1) rem 2), ?tarabish_BaitType_HALF, State);

process_hand(#state{hscore={S1, S2}, caller=0} = State) when S1 < S2 ->
  % bait
  process_hand(0, S1 + S2, ?tarabish_BaitType_FULL, State);

process_hand(#state{hscore={S1, S2}, caller=1} = State) when S2 < S1 ->
  % bait
  process_hand(S1 + S2, 0, ?tarabish_BaitType_FULL, State);

process_hand(#state{hscore={S1, S2}} = State) ->
  % normal
  process_hand(S1, S2, ?tarabish_BaitType_NONE, State).

process_hand(Score1, Score2, BaitType,
  #state{score=Score, caller=_Caller, table=Table} = State) ->

  HandScoreList = [Score1, Score2],
  ScoreList = tuple_to_list(Score),
  NewScoresList = lists:zipwith(fun(X, Y) -> X + Y end, HandScoreList, ScoreList),

  Event = #event{type=?tarabish_EventType_HAND_DONE,
                 hand_score=HandScoreList,
                 score=NewScoresList,
                 bait=BaitType},
  table:broadcast(Table, Event),

  process_scores(NewScoresList, State#state{score=list_to_tuple(NewScoresList)}).


process_trick(LastWin, #state{trick=9} = State) ->
  % TODO: add 20's and 50's
  State1 = add_hscore(LastWin, 10, State),

  process_hand(State1);

process_trick(LastWin, #state{trick=Trick} = State) ->
  Event = #event{type=?tarabish_EventType_ASK_CARD, seat=LastWin},
  table:broadcast(State#state.table, Event),

  {reply, ok, wait_card,
    new_trick(LastWin, State#state{trick = Trick + 1})}.

% First card of the trick
process_card(Seat, Card, Rest, #state{ledin=?tarabish_NONE} = State) ->
  process_card(Seat, Card, Rest, State#state{ledin=Card#card.suit});

process_card(Seat, Card, [], State) ->

  InPlay = lists:reverse([{Card, Seat}|State#state.inplay]),
  BestSeat = best_hand(InPlay, State#state.trump),
  Event = #event{type=?tarabish_EventType_TAKE_TRICK, seat=BestSeat},
  table:broadcast(State#state.table, Event),

  {Cards, _Seats} = lists:unzip(InPlay),
  CardScore = deck:score_cards(Cards, State#state.trump),
  State1 = add_hscore(BestSeat, CardScore, State),

  process_trick(BestSeat, State1);

process_card(Seat, Card, Rest, State) ->
      Event1 = #event{type=?tarabish_EventType_ASK_CARD, seat=hd(Rest)},
      table:broadcast(State#state.table, Event1),

      InPlay = [{Card, Seat}|State#state.inplay],
      {reply, ok, wait_card, State#state{order=Rest, inplay=InPlay}}.

to_RunType(twenty) -> ?tarabish_RunType_TWENTY;
to_RunType(fifty)  -> ?tarabish_RunType_FIFTY.

% Call run in the first trick only:
wait_card({call_run, Seat}, _From,
          #state{order=[Seat|_Rest], trick=1, runs=Runs} = State) ->
  case runs:call_run(Runs, Seat) of
    {{error, Reason}, Runs2} ->
      {reply, {error, Reason}, wait_card, State#state{runs=Runs2}};
    {Type, Runs2} ->
      EType = to_RunType(Type),
      Event = #event{type=?tarabish_EventType_CALL_RUN, seat=Seat, run=EType},
      table:broadcast(State#state.table, Event),
      {reply, ok, wait_card, State#state{runs=Runs2}}
  end;

% TODO: no way to show run in 3ed if missed in 2nd
wait_card({show_run, Seat}, _From,
          #state{order=[Seat|Rest], trick=2, runs=Runs, runShown=false} = State) ->
  case runs:show_run(Runs, Seat, Rest) of
    {{error, Reason}, Runs2} ->
      {reply, {error, Reason}, wait_card, State#state{runs=Runs2}};
    {{Type, Cards, Score}, Runs2} ->
      EType = to_RunType(Type),
      Event = #event{type=?tarabish_EventType_SHOW_RUN, seat=Seat,
                     run=EType, cards=Cards},
      table:broadcast(State#state.table, Event),
      State1 = add_hscore(Seat, Score, State),
      {reply, ok, wait_card, State1#state{runs=Runs2, runShown=true}};
    {{equal, Type, High, Trump, OtherSeat}, Runs2} ->
      EType = to_RunType(Type),
      Event = #event{type=?tarabish_EventType_NOSHOW_RUN, seat=Seat,
                     better=?tarabish_BetterType_EQUAL, run=EType,
                     high_value=High, is_trump=Trump, other_seat=OtherSeat},
      table:broadcast(State#state.table, Event),
      % Run counts for no one:
      {reply, ok, wait_card, State#state{runs=Runs2, runShown=true}};
    % TODO: should we always announce trump, or only when same high?
    {{better, Type, High, Trump, OtherSeat}, Runs2} ->
      EType = to_RunType(Type),
      Event = #event{type=?tarabish_EventType_NOSHOW_RUN, seat=Seat,
                     better=?tarabish_BetterType_BETTER, run=EType,
                     high_value=High, is_trump=Trump, other_seat=OtherSeat},
      table:broadcast(State#state.table, Event),
      % Not Shown, the better can still show:
      {reply, ok, wait_card, State#state{runs=Runs2}}
  end;

wait_card({play_card, Seat, #card{suit=Suit, value=Value} = Card}, _From,
    #state{order=[Seat|Rest], ledin=LedIn, trump=Trump, htrump=HighTrump} = State) ->
  Hand = element(Seat + 1, State#state.hands),

  case rules:valid_play(Card, Hand, LedIn, trump, HighTrump) of
    true ->

      Event = #event{type=?tarabish_EventType_PLAY_CARD, seat=Seat, card=Card},
      table:broadcast(State#state.table, Event),

      NewHand = lists:delete(Card, Hand),
      NewHands = setelement(Seat + 1, State#state.hands, NewHand),
      case Suit == Trump of
          true ->
            process_card(Seat, Card, Rest, State#state{hands=NewHands,
                htrump=Value});
          false ->
            process_card(Seat, Card, Rest, State#state{hands=NewHands})
        end;
    false ->
      {reply, {error, invalid}, wait_card, State}
  end;

wait_card(_Event, _From, State) ->
  {reply, {error, invalid}, wait_card, State}.

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
handle_event(stop, _StateName, StateData) ->
      {stop, normal, StateData};

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
new_hand(Dealer, State) when Dealer > 3 ->
  new_hand(Dealer rem 4, State);

new_hand(Dealer, #state{table=Table} = State) ->
  Deck = deck:shuffle(deck:new()),

  State1 = State#state{hands={[], [], [], []},
                 runs=undefined,
                 runShown=false,
                 deck=Deck,
                 dealer=Dealer,
                 trick=0,
                 trump=?tarabish_NONE,
                 hscore={0, 0},
                 caller=-1},

  State2 = deal3(State1),
  State3 = deal3(State2),
  State4 = new_trick(State3#state.dealer + 1, State3),

  FirstSeat = hd(State4#state.order),

  % TODO: add to_wait_trump() to send these events:
  AskTrumpEvent = #event{type=?tarabish_EventType_ASK_TRUMP,
                         seat=FirstSeat},
  table:broadcast(Table, AskTrumpEvent),

  State4.

new_trick(LastWin, State) ->
  DealOrder = create_order(LastWin),
  State#state{order=DealOrder,
              ledin=?tarabish_NONE,
              htrump=0,
              inplay=[]}.

add_hscore(Seat, Score, State) ->
  ScoreSeat = (Seat rem 2) + 1,
  OldScore = element(ScoreSeat, State#state.hscore),
  NewScores = setelement(ScoreSeat, State#state.hscore, OldScore + Score),
  State#state{hscore=NewScores}.

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

deal3_each(_Deck, Dealt, []) ->
  lists:reverse(Dealt);

deal3_each(Deck,  Dealt, [_Seat|Others]) ->
  {Cards, Deck1} = lists:split(3, Deck),
  deal3_each(Deck1, [Cards|Dealt], Others).

deal3(State) ->
  Order = create_order(State#state.dealer + 1),

  BeforeCards = tuple_to_list(State#state.hands),
  NewCards = deal3_each(State#state.deck, [], Order),
  Cards = lists:zipwith(fun lists:merge/2, BeforeCards, NewCards),

  table:deal3(State#state.table, State#state.dealer, NewCards),

  State#state{deck=lists:nthtail(12, State#state.deck),
              hands=list_to_tuple(Cards)}.

create_order(First) when First > 3 ->
  create_order(First rem 4);

create_order(0) ->
  lists:seq(0, 3);

create_order(First) ->
  lists:seq(First, 3) ++ lists:seq(0, First - 1).

best_hand([{FirstCard, FirstSeat}|Rest], Trump) ->
  best_hand(Rest, {FirstCard, FirstSeat}, Trump, FirstCard#card.suit).

best_hand([], {_Card, Seat}, _Trump, _Led) ->
  Seat;

best_hand([{NewCard, NewSeat}|Rest], {Card, _Seat}, Trump, Led)
  when NewCard#card.suit == Trump, Card#card.suit /= Trump ->
    best_hand(Rest, {NewCard, NewSeat}, Trump, Led);

best_hand([{NewCard, _NewSeat}|Rest], {Card, Seat}, Trump, Led)
  when NewCard#card.suit /= Trump, Card#card.suit == Trump ->
    best_hand(Rest, {Card, Seat}, Trump, Led);

best_hand([{NewCard, NewSeat}|Rest], {Card, Seat}, Trump, Led)
  when NewCard#card.suit == Trump, Card#card.suit == Trump ->
    case deck:trump_higher(NewCard#card.value, Card#card.value) of
      true -> best_hand(Rest, {NewCard, NewSeat}, Trump, Led);
      false -> best_hand(Rest, {Card, Seat}, Trump, Led)
    end;

best_hand([{NewCard, _NewSeat}|Rest], {Card, Seat}, Trump, Led)
  when NewCard#card.suit /= Led ->
    best_hand(Rest, {Card, Seat}, Trump, Led);

best_hand([{NewCard, NewSeat}|Rest], {Card, Seat}, Trump, Led) ->
  case deck:nontrump_higher(NewCard#card.value, Card#card.value) of
    true -> best_hand(Rest, {NewCard, NewSeat}, Trump, Led);
    false -> best_hand(Rest, {Card, Seat}, Trump, Led)
  end.

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

create_order_test_() ->
  [
    ?_assertEqual([0,1,2,3], create_order(0)),
    ?_assertEqual([1,2,3,0], create_order(1)),
    ?_assertEqual([2,3,0,1], create_order(2)),
    ?_assertEqual([3,0,1,2], create_order(3)),
    ?_assertEqual([0,1,2,3], create_order(4)),
    ?_assertEqual([1,2,3,0], create_order(5))
  ].

best_hand_test_() ->
  Hands1 = [{#card{value=8, suit=?tarabish_SPADES},   0},
            {#card{value=8, suit=?tarabish_DIAMONDS}, 1},
            {#card{value=8, suit=?tarabish_HEARTS},   2},
            {#card{value=8, suit=?tarabish_CLUBS},    3}],

  Hands2 = [{#card{value=8, suit=?tarabish_SPADES},   2},
            {#card{value=9, suit=?tarabish_SPADES},   3},
            {#card{value=6, suit=?tarabish_HEARTS},   0},
            {#card{value=6, suit=?tarabish_CLUBS},    1}],

  Hands3 = [{#card{value=8,  suit=?tarabish_SPADES},   3},
            {#card{value=8,  suit=?tarabish_DIAMONDS}, 0},
            {#card{value=9,  suit=?tarabish_DIAMONDS}, 1},
            {#card{value=?tarabish_JACK, suit=?tarabish_DIAMONDS}, 2}],

  [
    ?_assertEqual(0, best_hand(Hands1, ?tarabish_SPADES)),
    ?_assertEqual(2, best_hand(Hands1, ?tarabish_HEARTS)),
    ?_assertEqual(3, best_hand(Hands2, ?tarabish_DIAMONDS)),
    ?_assertEqual(2, best_hand(Hands3, ?tarabish_DIAMONDS))
  ].
