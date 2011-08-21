-module(runs).

-include("tarabish_constants.hrl").
-include("tarabish_types.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(best_run, {type, high, score, trump}).
-record(run_record, {twenty, fifty, bella, best_run, score}).

-export([new/2]).

new(Hands, TrumpSuit) ->
  Runs = lists:map(fun (Hand) -> new_run_record(Hand, TrumpSuit) end, Hands),
  Runs.

new_run_record(Cards, TrumpSuit) ->
  SortedCards = sort_cards_by_value(Cards),
  SplitBySuit = split_by_suit(SortedCards),
  {CardsBySuit, Suits} = lists:unzip(SplitBySuit),

  % {suit, {type, high, score}}
  Runs = lists:zip(Suits, lists:map(fun score_suit/1, CardsBySuit)),

  Folder = fun({Suit, Run}, Acc) -> fold_runs(Suit, Run, Acc, TrumpSuit) end,
  NoRun = {nosuit, #best_run{type=none, high=0, score=0, trump=false}},

  _BestRun = lists:foldl(Folder, NoRun, Runs),

  #run_record{twenty=false,
              fifty=false,
              bella=false,
              best_run=none,
              score=0}.

% This sorts top to bottom by values to make run finding easier
sort_cards_by_value2(#card{suit=Suit, value=V1},
                    #card{suit=Suit, value=V2}) ->
  not(V1 =< V2);

sort_cards_by_value2(#card{suit=Suit1}, #card{suit=Suit2}) ->
  Suit1 < Suit2.

sort_cards_by_value(Cards) ->
  lists:sort(fun sort_cards_by_value2/2, Cards).

split_by_suit([], Cards, Suit, Lists) ->
  lists:reverse([{lists:reverse(Cards), Suit}|Lists]);

split_by_suit([#card{suit=Suit} = Card|Rest], Cards, Suit, Lists) ->
  split_by_suit(Rest, [Card|Cards], Suit, Lists);

split_by_suit([Card|Rest], Cards, Suit, Lists) ->
  split_by_suit(Rest, [Card], Card#card.suit, [{Cards, Suit}|Lists]).

% Stable split to keep sorted
split_by_suit([Card|Rest]) ->
  split_by_suit(Rest, [Card], Card#card.suit, []).

make_best_run(Type, High, Score) ->
  #best_run{type=Type, high=High, score=Score, trump=false}.

% The last run, if it was a fifty, would be counted already
% Count the last run, it's the best twenty:
score_suit(_ExpectedValue, FirstValue, [], 3, none, _High, Score) ->
  make_best_run(twenty, FirstValue, Score + 20);

% Count the last twenty, but it's not the bext:
score_suit(_ExpectedValue, _FirstValue, [], 3, Other, High, Score) ->
  make_best_run(Other, High, Score + 20);

% No last run, return bests:
score_suit(_ExpectedValue, _FirstValue, [], _NoCount, Run, High, Score) ->
  make_best_run(Run, High, Score);

% Start of new run:
score_suit(_ExpectedValue, _FirstValue, [NewCard|Rest], 0, Run, High, Score) ->
  score_suit(NewCard#card.value - 1, NewCard#card.value, Rest, 1, Run, High, Score);

% New fifty, must be smaller (due to high-low sort):
score_suit(ExpectedValue, _FirstValue,
           [#card{value=ExpectedValue}|Rest], 3, fifty, High, Score) ->
  score_suit(none, none, Rest, 0, fifty, High, Score + 50);

% First fifty, doesn't matter if there's twenties:
score_suit(ExpectedValue, FirstValue,
           [#card{value=ExpectedValue}|Rest],
           3, _NotFifty, _High, Score) ->
    score_suit(none, none, Rest, 0, fifty, FirstValue, Score + 50);

% First twenty, and it's the best:
score_suit(_ExpectedValue, FirstValue, Cards, 3, none, _High, Score) ->
  score_suit(none, none, Cards, 0, twenty, FirstValue, Score + 20);

% Higher twenty or fifty, but count the score:
score_suit(_ExpectedValue, _FirstValue, Cards, 3, Run, High, Score) ->
  score_suit(none, none, Cards, 0, Run, High, Score + 20);

% Building a run:
score_suit(ExpectedValue, FirstValue,
           [#card{value=ExpectedValue}|Rest], Count,
           Run, High, Score) ->
    score_suit(ExpectedValue - 1, FirstValue, Rest, Count + 1, Run, High, Score);

% No run, start again, doesn't matter the count:
score_suit(_ExpectedValue, _FirstValue, Cards, _Count, Run, High, Score) ->
  score_suit(none, none, Cards, 0, Run, High, Score).

% Must be all the same suit:
score_suit(Cards) ->
  score_suit(none, none, Cards, 0, none, 0, 0).

% fifty better than other:
fold_runs(ThisSuit, #best_run{type=fifty, high=High, score=Score1},
          #best_run{type=Type, score=Score} = Best, Trump) when Type =/= fifty ->
  Best#best_run{type=fifty, score=Score + Score1, high=High,
    trump=(ThisSuit==Trump)};

% twenty when no other run:
fold_runs(ThisSuit, #best_run{type=twenty, high=High, score=Score1},
          #best_run{type=none, score=Score} = Best, Trump) ->
  Best#best_run{type=twenty, score=Score + Score1, high=High,
    trump=(ThisSuit==Trump)};

% Same high card, same type, but this is trump:
fold_runs(Trump, #best_run{type=Type, high=High, score=Score1},
          #best_run{type=Type, high=High, score=Score} = Best, Trump) ->
  Best#best_run{score=Score + Score1, trump=true};

% Better high card, same type:
fold_runs(_AnySuit, #best_run{type=Type, high=High, score=Score1},
          #best_run{type=Type, high=High2, score=Score} = Best, _Trump) when
          High > High2 ->
  Best#best_run{score=Score + Score1, high=High};

% Worse run:
fold_runs(_AnySuit, #best_run{score=Score1},
          #best_run{score=Score} = Best, _Trump) ->
  Best#best_run{score=Score + Score1}.

%%
%% Tests
%%

sort_by_value_test() ->
  Deck  = deck:new(),
  DeckS = deck:shuffle(Deck),

  Sorted = sort_cards_by_value(DeckS),

  [#card{value=?tarabish_ACE, suit=1}|Rest] = Sorted,
  [#card{value=?tarabish_KING, suit=1}|_Rest2] = Rest.

% J, Q, K, A --> 11, 12, 13, 14
score_suit_test_() ->
  M = fun (Value) -> #card{value=Value} end,
  ML = fun (VList) -> lists:map(M, VList) end,
  MB = fun ({Type, High, Score}) -> make_best_run(Type, High, Score) end,

  [
    ?_assertEqual(MB({none, 0, 0}), score_suit(ML([12, 10, 9, 7, 6]))),
    ?_assertEqual(MB({none, 0, 0}), score_suit(ML([12]))),

    ?_assertEqual(MB({twenty, 8, 20}), score_suit(ML([8, 7, 6]))),
    ?_assertEqual(MB({twenty, 8, 20}), score_suit(ML([13, 12, 10, 8, 7, 6]))),
    ?_assertEqual(MB({twenty, 13, 20}), score_suit(ML([13, 12, 11, 9, 7, 6]))),

    % Double twenty:
    ?_assertEqual(MB({twenty, 14, 40}), score_suit(ML([14, 13, 12, 10, 9, 8, 6]))),

    ?_assertEqual(MB({fifty, 9,  50}), score_suit(ML([9, 8, 7, 6]))),
    ?_assertEqual(MB({fifty, 12, 70}), score_suit(ML([12, 11, 10, 9, 8, 7, 6]))),

    % Double fifty:
    ?_assertEqual(MB({fifty, 14, 100}),
      score_suit(ML([14, 13, 12, 11, 10, 9, 8, 7, 6])))
  ].

fold_runs_test_() ->
  FiftyA50 = #best_run{type=fifty, high=14, score=50, trump=false},
  FiftyJ50 = #best_run{type=fifty, high=11, score=50, trump=false},

  TwentyA  = #best_run{type=twenty, high=14, score=20, trump=false},

  Best50NT = #best_run{type=fifty, high=14, score=50, trump=false},
  Best20NT = #best_run{type=twenty, high=14, score=40, trump=false},

%-record(best_run, {type, high, score, trump}).
  [
    % Test Trump equal beats non-trump:
    ?_assertEqual(Best50NT#best_run{score=100, trump=true},
      fold_runs(trump, FiftyA50, Best50NT, trump)),

    ?_assertEqual(Best20NT#best_run{score=60, trump=true},
      fold_runs(trump, TwentyA, Best20NT, trump)),

    % Test lower trump does not:
    ?_assertEqual(Best50NT#best_run{score=100},
      fold_runs(trump, FiftyJ50, Best50NT, trump)),

    % Fifty beats twenty, even if twenty was trump and had a high card
    ?_assertEqual(Best50NT#best_run{high=11, score=90},
      fold_runs(nontrump, FiftyJ50, Best20NT#best_run{trump=true}, trump)),

    % Twenty doesn't beat fifty, even trump with higher card:
    ?_assertEqual(Best50NT#best_run{high=11, score=70},
      fold_runs(trump, TwentyA, Best50NT#best_run{high=11}, trump))

  ].
