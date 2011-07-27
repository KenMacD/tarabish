-module(rules).

%%
%% Include files
%%
-include("tarabish_constants.hrl").
-include("tarabish_types.hrl").

-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([valid_play/5]).

valid_play(Card, Hand, Led, TrumpSuit, HighTrumpValue) ->
  ValidCards = find_must(Hand, Led, TrumpSuit, HighTrumpValue),
  case ValidCards == [] of
    true  -> lists:member(Card, Hand);
    false -> lists:member(Card, ValidCards)
  end.

insuit(Suit) -> fun(Card) -> Card#card.suit == Suit end.

% Follow suit if you can, otherwise play trump.
find_must(Hand, Led, Trump, HighTrumpValue) ->
  Cards = best_list(Hand, [insuit(Led), insuit(Trump)]),
  find_must_trump(Cards, Trump, HighTrumpValue).

% If our limited cards are trump, filter them to include only those above HighTrumpValue
find_must_trump([#card{suit=Trump}|_Rest] = Cards, Trump, HighTrumpValue) ->
  HighTrump = fun(Card) -> deck:trump_higher(Card#card.value, HighTrumpValue) end,
  Higher = lists:filter(HighTrump, Cards),

  case Higher == [] of
    true  -> Cards;
    false -> Higher
  end;

find_must_trump(Cards, _Trump, _HighTrumpValue) ->
  Cards.

best_list(Hand, []) ->
  Hand;

best_list(Hand, [Pred|Rest]) ->
  Result = lists:filter(Pred, Hand),
  case Result == [] of
    true ->
      best_list(Hand, Rest);
    false ->
      Result
  end.


%%
%% Tests
%%
card(Value, Suit) ->
  #card{value=Value, suit=Suit}.

in_suit_test_() ->
  Hand = [card(6, ?tarabish_SPADES),
          card(7, ?tarabish_SPADES),
          card(8, ?tarabish_SPADES),
          card(9, ?tarabish_SPADES),
          card(6, ?tarabish_CLUBS),
          card(7, ?tarabish_CLUBS),
          card(8, ?tarabish_CLUBS),
          card(9, ?tarabish_CLUBS),
          card(6, ?tarabish_DIAMONDS)],
  [
    % Follow suit:
    ?_assertEqual(true, valid_play(card(6, ?tarabish_SPADES), Hand,
        ?tarabish_SPADES, ?tarabish_DIAMONDS, 0)),
    ?_assertEqual(false, valid_play(card(6, ?tarabish_CLUBS), Hand,
        ?tarabish_SPADES, ?tarabish_DIAMONDS, 0)),

    % or trump:
    ?_assertEqual(true, valid_play(card(6, ?tarabish_DIAMONDS), Hand,
        ?tarabish_HEARTS, ?tarabish_DIAMONDS, 0)),
    ?_assertEqual(false, valid_play(card(6, ?tarabish_SPADES), Hand,
        ?tarabish_HEARTS, ?tarabish_DIAMONDS, 0)),

    % but high trump:
    ?_assertEqual(false, valid_play(card(6, ?tarabish_SPADES), Hand,
        ?tarabish_SPADES, ?tarabish_SPADES, 10)),
    ?_assertEqual(true, valid_play(card(9, ?tarabish_SPADES), Hand,
        ?tarabish_SPADES, ?tarabish_SPADES, 0)),

    % no good cards, play anything (that you have):
    ?_assertEqual(true, valid_play(card(6, ?tarabish_SPADES), Hand,
        ?tarabish_HEARTS, ?tarabish_HEARTS, 0)),
    ?_assertEqual(true, valid_play(card(9, ?tarabish_SPADES), Hand,
        ?tarabish_HEARTS, ?tarabish_HEARTS, 0)),
    ?_assertEqual(true, valid_play(card(9, ?tarabish_CLUBS), Hand,
        ?tarabish_HEARTS, ?tarabish_HEARTS, 0)),
    ?_assertEqual(true, valid_play(card(6, ?tarabish_DIAMONDS), Hand,
        ?tarabish_HEARTS, ?tarabish_HEARTS, 0)),
    ?_assertEqual(false, valid_play(card(6, ?tarabish_HEARTS), Hand,
        ?tarabish_HEARTS, ?tarabish_HEARTS, 0)),
    ?_assertEqual(false, valid_play(card(?tarabish_JACK, ?tarabish_HEARTS), Hand,
        ?tarabish_HEARTS, ?tarabish_HEARTS, 0))

  ].
