-module(runs).

-include("tarabish_constants.hrl").
-include("tarabish_types.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(best_run, {type, high, trump}).
-record(run_record, {best_run, score, to_show, called, bella}).

-export([new/2, call_run/2, show_run/3, has_bella/2]).

new(Hands, TrumpSuit) ->
  {_Count, Dict} = lists:foldl(fun(Hand, {Count, Dict}) -> {Count + 1,
          orddict:store(Count, new_run_record(Hand, TrumpSuit), Dict)} end,
          {0, orddict:new()}, Hands),
  Dict.

call_run(Runs, Seat) ->
  Record = orddict:fetch(Seat, Runs),
  {Reply, Record1} = call_run(Record),
  {Reply, orddict:store(Seat, Record1, Runs)}.

call_run(#run_record{called=true} = Record) ->
  {{error, alread_called}, Record};

call_run(#run_record{best_run=#best_run{type=none}} = Record) ->
  {{error, no_run}, Record};

call_run(#run_record{best_run=#best_run{type=Type}} = Record) ->
  {Type, Record#run_record{called=true}}.

show_run(Runs, Seat, AfterSeats) ->
  Record = orddict:fetch(Seat, Runs),
  OtherRecords = lists:map(fun (Num) -> {Num, orddict:fetch(Num, Runs)} end, AfterSeats),
  Reply = show_run(Record, OtherRecords),
  {Reply, Runs}.

show_run(#run_record{best_run=#best_run{type=none}}, _Other) ->
  {error, no_run};

show_run(#run_record{called=false}, _Other) ->
  {error, didnt_call};

% No one to left that could be better
show_run(#run_record{best_run=#best_run{type=Type}, score=Score, to_show=Cards}, []) ->
  {Type, Cards, Score};

show_run(#run_record{best_run=Best} = Record,
         [{OSeat, #run_record{best_run=BestOther}}|Rest]) ->
  case compare_best(BestOther, Best) of
    worse ->
      show_run(Record, Rest);
    better ->
      {better, Best#best_run.type, Best#best_run.high, Best#best_run.trump, OSeat};
    equal ->
      {equal, Best#best_run.type, Best#best_run.high, Best#best_run.trump, OSeat}
  end.

has_bella(Runs, Seat) ->
  Record = orddict:fetch(Seat, Runs),
  Record#run_record.bella == true.

% Same, both non-trump
compare_best(#best_run{type=Type, high=High, trump=false},
             #best_run{type=Type, high=High, trump=false}) ->
  equal;

% Same, one trump
compare_best(#best_run{type=Type, high=High, trump=Trump},
             #best_run{type=Type, high=High}) ->
  case Trump of
    true -> better;
    false -> worse
  end;

% Same type, higher card
compare_best(#best_run{type=Type, high=High},
             #best_run{type=Type, high=High2}) ->
  case High > High2 of
    true -> better;
    false -> worse
  end;

% different type - same type handled above
compare_best(#best_run{type=fifty}, _Other) -> better;
compare_best(_Other, #best_run{type=fifty}) -> worse;
compare_best(#best_run{type=twenty}, _Other) -> better;
compare_best(_Other, #best_run{type=twenty}) -> worse.

bella_in_hand([], _Trump, 2) ->
  true;

bella_in_hand([], _Trump, _Other) ->
  false;

bella_in_hand([#card{suit=Trump, value=?tarabish_KING}|Rest], Trump, Count) ->
  bella_in_hand(Rest, Trump, Count + 1);

bella_in_hand([#card{suit=Trump, value=?tarabish_QUEEN}|Rest], Trump, Count) ->
  bella_in_hand(Rest, Trump, Count + 1);

bella_in_hand([_Card|Rest], Trump, Count) ->
  bella_in_hand(Rest, Trump, Count).

bella_in_hand(Cards, Trump) ->
  bella_in_hand(Cards, Trump, 0).

new_run_record(Cards, TrumpSuit) ->
  SortedCards = sort_cards_by_value(Cards),
  CardsBySuit = split_by_suit(SortedCards),

  SplitByRuns = lists:reverse(lists:foldl(fun lists:append/2, [],
    lists:map(fun split_runs/1, CardsBySuit))),

  SortBySize = sort_runs(SplitByRuns, TrumpSuit),

  CardsToShow = lists:flatten(SortBySize),
  Score = score_runs(SplitByRuns),
  BestRun = make_best(SortBySize, TrumpSuit),

  Bella = bella_in_hand(Cards, TrumpSuit),
  % TODO: look for bella in CardsToShow to see if you can count early

  #run_record{best_run=BestRun,
              score=Score,
              to_show=CardsToShow,
              called=false,
              bella=Bella}.

% This sorts top to bottom by values to make run finding easier
sort_cards_by_value2(#card{suit=Suit, value=V1},
                    #card{suit=Suit, value=V2}) ->
  not(V1 =< V2);

sort_cards_by_value2(#card{suit=Suit1}, #card{suit=Suit2}) ->
  Suit1 < Suit2.

sort_cards_by_value(Cards) ->
  lists:sort(fun sort_cards_by_value2/2, Cards).

sort_runs(Run1, Run2, _Trump)
  when length(Run1) =/= length(Run2) ->
    not(length(Run1) =< length(Run2));

sort_runs([#card{value=V1}|_], [#card{value=V2}|_], _Trump)
  when V1 =/= V2 ->
    not(V1 =< V2);

sort_runs(_Run1, [#card{suit=S1}|_], Trump) ->
  not(S1 == Trump).

sort_runs(Runs, Trump) ->
  lists:sort(fun (X, Y) -> sort_runs(X, Y, Trump) end, Runs).

make_best([], _Trump) ->
  #best_run{type=none, high=0, trump=false};

make_best([[#card{value=V1, suit=S1}|_] = Run |_], Trump) when length(Run) == 4 ->
  #best_run{type=fifty, high=V1, trump=(S1 == Trump)};

make_best([[#card{value=V1, suit=S1}|_]|_], Trump) ->
  #best_run{type=twenty, high=V1, trump=(S1 == Trump)}.

score_runs([], Score) ->
  Score;

score_runs([Run|Rest], Score) when length(Run) == 4 ->
  score_runs(Rest, Score + 50);

score_runs([_Run|Rest], Score) ->
  score_runs(Rest, Score + 20).

score_runs(Runs) ->
  score_runs(Runs, 0).

split_by_suit([], Cards, _Suit, Lists) ->
  lists:reverse([lists:reverse(Cards)|Lists]);

split_by_suit([#card{suit=Suit} = Card|Rest], Cards, Suit, Lists) ->
  split_by_suit(Rest, [Card|Cards], Suit, Lists);

split_by_suit([Card|Rest], Cards, _Suit, Lists) ->
  split_by_suit(Rest, [Card], Card#card.suit, [lists:reverse(Cards)|Lists]).

% Stable split to keep sorted
split_by_suit([Card|Rest]) ->
  split_by_suit(Rest, [Card], Card#card.suit, []).

% Split them into 4's and 3's:

% Count last run:
split_runs([], ThisRun, Runs) when length(ThisRun) > 2 ->
  lists:reverse([lists:reverse(ThisRun)|Runs]);

split_runs([], _ThisRun, Runs) ->
  lists:reverse(Runs);

% Have 4
split_runs([Card|Rest], ThisRun, Runs) when length(ThisRun) == 4 ->
  split_runs(Rest, [Card], [lists:reverse(ThisRun)|Runs]);

split_runs([#card{value=Value1} = Card|Rest],
         [#card{value=Value2}|_Rest2] = ThisRun, Runs) when Value1 == Value2 - 1 ->
    split_runs(Rest, [Card|ThisRun], Runs);

split_runs([Card|Rest], ThisRun, Runs) when length(ThisRun) == 3 ->
  split_runs(Rest, [Card], [lists:reverse(ThisRun)|Runs]);

split_runs([Card|Rest], _ThisRun, Runs) ->
  split_runs(Rest, [Card], Runs).

split_runs([]) ->
  [];

split_runs([Card|Rest]) ->
    split_runs(Rest, [Card], []).

%%
%% Tests
%%

sort_by_value_test() ->
  Deck  = deck:new(),
  DeckS = deck:shuffle(Deck),

  Sorted = sort_cards_by_value(DeckS),

  [#card{value=?tarabish_ACE, suit=1}|Rest] = Sorted,
  [#card{value=?tarabish_KING, suit=1}|_Rest2] = Rest.

sort_runs_test_() ->
  M  = fun (Value) -> #card{value=Value, suit=1} end,
  MT = fun (Value) -> #card{value=Value, suit=2} end,
  ML = fun (VList) -> lists:map(M, VList) end,
  MLT= fun (VList) -> lists:map(MT, VList) end,

  [
    ?_assertEqual([ML([10, 9, 8, 7]), ML([9, 8, 7, 6]), ML([14, 13, 12])],
      sort_runs([ML([14, 13, 12]), ML([9, 8, 7, 6]), ML([10, 9, 8, 7])], 2)),
    ?_assertEqual([MLT([10, 9, 8, 7]), ML([10, 9, 8, 7]), ML([14, 13, 12])],
      sort_runs([ML([14, 13, 12]), ML([10, 9, 8, 7]), MLT([10, 9, 8, 7])], 2))
  ].

new_run_record_test_() ->
  C = fun(V, S) -> #card{value=V, suit=S} end,

  NoBest = #best_run{type=none, high=0, trump=false},

  Twenty = #best_run{type=twenty, high=14, trump=false},
  TwentyCards = [C(14, 0), C(13, 0), C(12,0),
                 C(14, 1), C(13, 1), C(12,1),
                 C(14, 2), C(13, 2), C(12,2)],

  Fifty = #best_run{type=fifty, high=13, trump=false},
  FiftyCards = [C(13, 1), C(12, 1), C(11, 1), C(10,1),
                C(14, 2), C(13, 2), C(12, 2)],

  EmptyRR = #run_record{best_run=NoBest, score=0, to_show=[], called=false, bella=false},
  FiftyT = Fifty#best_run{trump=true},
  [
    ?_assertEqual(EmptyRR,
      new_run_record([C(14, 0), C(14, 1), C(14, 2), C(14, 3),
                      C(12, 0), C(12, 1), C(12, 2), C(12, 3),
                      C(10, 0), C(10, 1), C(10, 2), C(10, 3)], 0)),
    ?_assertEqual(EmptyRR#run_record{best_run=Twenty, score=60, to_show=TwentyCards},
      new_run_record([C(14, 0), C(14, 1), C(14, 2), C(14, 3),
                      C(13, 0), C(13, 1), C(13, 2), C(12, 3),
                      C(12, 0), C(12, 1), C(12, 2), C(10, 3)], 3)),
    % The twenty is trump, but the fifty still wins (and we have bells):
    ?_assertEqual(EmptyRR#run_record{best_run=Fifty, score=70,
        to_show=FiftyCards, bella=true},
      new_run_record([C(14, 0), C(08, 0), C(11, 1), C(13, 2),
                      C(12, 0), C(13, 1), C(10, 1), C(12, 2),
                      C(10, 0), C(12, 1), C(14, 2), C(10, 3)], 2)),
    ?_assertEqual(EmptyRR#run_record{best_run=FiftyT, score=70,
        to_show=FiftyCards, bella=true },
      new_run_record([C(14, 0), C(08, 0), C(11, 1), C(13, 2),
                      C(12, 0), C(13, 1), C(10, 1), C(12, 2),
                      C(10, 0), C(12, 1), C(14, 2), C(10, 3)], 1))
  ].

split_runs_test_() ->
  M = fun (Value) -> #card{value=Value} end,
  ML = fun (VList) -> lists:map(M, VList) end,

  [
    ?_assertEqual([ML([12, 11, 10]), ML([8, 7, 6])],
      split_runs(ML([14, 12, 11, 10, 8, 7, 6]))),
    ?_assertEqual([ML([14, 13, 12, 11]), ML([9, 8, 7, 6])],
      split_runs(ML([14, 13, 12, 11, 9, 8, 7, 6]))),
    ?_assertEqual([],
      split_runs(ML([14, 13, 11, 10, 8, 7])))
  ].
