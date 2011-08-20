-module(runs).

-include("tarabish_constants.hrl").
-include("tarabish_types.hrl").

-include_lib("eunit/include/eunit.hrl").

-record(run_record, {twenty, fifty, bella, best_run, score}).

-export([new/2]).

new(Hands, TrumpSuit) ->
  Runs = lists:map(fun (Hand) -> new_run_record(Hand, TrumpSuit) end, Hands),
  Runs.

new_run_record(Cards, _TrumpSuit) ->
  SortedCards = sort_cards_by_value(Cards),
  SplitBySuit = split_by_suit(SortedCards),
  _Runs = lists:map(fun score_suit/1, SplitBySuit),
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

split_by_suit([], Cards, _Suit, Lists) ->
  lists:reverse([lists:reverse(Cards)|Lists]);

split_by_suit([#card{suit=Suit} = Card|Rest], Cards, Suit, Lists) ->
  split_by_suit(Rest, [Card|Cards], Suit, Lists);

split_by_suit([Card|Rest], Cards, _Suit, Lists) ->
  split_by_suit(Rest, [Card], Card#card.suit, [Cards|Lists]).

% Stable split to keep sorted
split_by_suit([Card|Rest]) ->
  split_by_suit(Rest, [Card], Card#card.suit, []).

% The last run, if it was a fifty, would be counted already
% Count the last run, it's the best twenty:
score_suit(_ExpectedValue, FirstValue, [], 3, none, _High, Score) ->
  {twenty, FirstValue, Score + 20};

% Count the last twenty, but it's not the bext:
score_suit(_ExpectedValue, _FirstValue, [], 3, Other, High, Score) ->
  {Other, High, Score + 20};

% No last run, return bests:
score_suit(_ExpectedValue, _FirstValue, [], _NoCount, Run, High, Score) ->
  {Run, High, Score};

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

  [
    ?_assertEqual({none, 0, 0}, score_suit(ML([12, 10, 9, 7, 6]))),
    ?_assertEqual({none, 0, 0}, score_suit(ML([12]))),

    ?_assertEqual({twenty, 8, 20}, score_suit(ML([8, 7, 6]))),
    ?_assertEqual({twenty, 8, 20}, score_suit(ML([13, 12, 10, 8, 7, 6]))),
    ?_assertEqual({twenty, 13, 20}, score_suit(ML([13, 12, 11, 9, 7, 6]))),

    % Double twenty:
    ?_assertEqual({twenty, 14, 40}, score_suit(ML([14, 13, 12, 10, 9, 8, 6]))),

    ?_assertEqual({fifty, 9,  50}, score_suit(ML([9, 8, 7, 6]))),
    ?_assertEqual({fifty, 12, 70}, score_suit(ML([12, 11, 10, 9, 8, 7, 6]))),

    % Double fifty:
    ?_assertEqual({fifty, 14, 100},
      score_suit(ML([14, 13, 12, 11, 10, 9, 8, 7, 6])))
  ].
