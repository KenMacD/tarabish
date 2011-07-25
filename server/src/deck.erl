-module(deck).

%%
%% Include files
%%
-include("tarabish_constants.hrl").
-include("tarabish_types.hrl").

-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([new/0, shuffle/1, high_card/1, nontrump_higher/2, trump_higher/2]).

%%
%% API Functions
%%
new() ->
  Values = lists:seq(6, ?tarabish_ACE),
  Suits = lists:seq(1, 4),
  [#card{value=V, suit=S} || V <- Values, S <- Suits].

% Taken from: http://www.trapexit.org/RandomShuffle
shuffle(Cards) ->
%% Determine the log n portion then randomize the list.
  randomize(round(math:log(length(Cards)) + 0.5), Cards).

randomize(1, List) ->
  randomize(List);

randomize(T, List) ->
  lists:foldl(fun(_E, Acc) ->
                 randomize(Acc)
              end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
  D = lists:map(fun(A) ->
                   {random:uniform(), A}
            end, List),
  {_, D1} = lists:unzip(lists:keysort(1, D)),
  D1.

% Rank the cards for dealer (as if they're all trump)

% Moves J 9 A 10 above others (number have no meaning, just above 14):
trump_rank(?tarabish_JACK) ->
  20;
trump_rank(9) ->
  19;
trump_rank(?tarabish_ACE) ->
  18;
trump_rank(10) ->
  17;
trump_rank(Other) ->
  Other.

high_card(Cards) when is_list(Cards) ->
  [First|Rest] = lists:map(fun(Card) -> trump_rank(Card#card.value) end, Cards),
  high_card(Rest, First, [0], 1).

high_card([], _Highest, Winners, _On) ->
  lists:reverse(Winners);

high_card([Value|Rest], Max, Winners, On) when Value =:= Max ->
  high_card(Rest, Max, [On|Winners], On + 1);

high_card([Value|Rest], Max, _OldWinners, On) when Value > Max ->
  high_card(Rest, Value, [On], On + 1);

high_card([_Value|Rest], Max, Winners, On) -> % lower
  high_card(Rest, Max, Winners, On + 1).

nontrump_higher(?tarabish_ACE, _Value) ->
  true;

nontrump_higher(_Value, ?tarabish_ACE) ->
  false;

nontrump_higher(10, _Value) ->
  true;

nontrump_higher(_Value, 10) ->
  false;

nontrump_higher(Value1, Value2) when Value1 > Value2 ->
  true;

nontrump_higher(_Value1, _Value2) ->
  false.

trump_higher(?tarabish_JACK, _Value) ->
  true;

trump_higher(_Value, ?tarabish_JACK) ->
  false;

trump_higher(9, _Value) ->
  true;

trump_higher(_Value, 9) ->
  false;

trump_higher(Value1, Value2) ->
  nontrump_higher(Value1, Value2).


%%
%% Local Functions
%%

%%
%% Tests
%%
create_test() ->
  Deck = new(),
  36 = length(Deck),
  [#card{value=6, suit=1}|_Rest] = Deck,
  #card{value=?tarabish_ACE, suit=4} = lists:last(Deck).

high_card_test() ->
  J = #card{value=?tarabish_JACK},
  N = #card{value=9},
  A = #card{value=?tarabish_ACE},
  T = #card{value=10},
  ?assertEqual([0],    high_card([J, N, A, T])),
  ?assertEqual([0, 1], high_card([J, J, N, A])),
  ?assertEqual([1, 3], high_card([N, J, N, J])).

high_trump_test_() ->
  [
    ?_assertEqual(true,  trump_higher(?tarabish_JACK, 8)),
    ?_assertEqual(false, trump_higher(9, ?tarabish_JACK)),
    ?_assertEqual(true,  trump_higher(?tarabish_JACK, 9)),
    ?_assertEqual(true,  trump_higher(9, ?tarabish_ACE)),
    ?_assertEqual(true,  trump_higher(?tarabish_ACE, 10)),
    ?_assertEqual(true, trump_higher(10, ?tarabish_KING)),
    ?_assertEqual(true, trump_higher(8, 7))
  ].

high_nontrump_test_() ->
  [
    ?_assertEqual(true, nontrump_higher(?tarabish_ACE, 10)),
    ?_assertEqual(true, nontrump_higher(10, ?tarabish_KING)),
    ?_assertEqual(true, nontrump_higher(?tarabish_KING, ?tarabish_JACK)),
    ?_assertEqual(true, nontrump_higher(9, 8))
  ].
