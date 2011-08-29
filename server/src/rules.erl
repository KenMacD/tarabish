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
-export([valid_play/5, valid_bella/5]).

% When nothing led you can play anything:
valid_play(Card, Hand, ?tarabish_NONE, _TrumpSuit, _HighTrumpValue) ->
  lists:member(Card, Hand);

valid_play(Card, Hand, Led, TrumpSuit, HighTrumpValue) ->
  ValidCards = find_must(Hand, Led, TrumpSuit, HighTrumpValue),
  case ValidCards == [] of
    true  -> lists:member(Card, Hand);
    false -> lists:member(Card, ValidCards)
  end.

% We only look at cards remaining here. That the player originally had bella
% must be checked from the runs (when we have the full hand)
valid_bella(#card{suit=TrumpSuit} = Card, Hand, Led, TrumpSuit, HighTrumpValue) ->
  case valid_play(Card, Hand, Led, TrumpSuit, HighTrumpValue) of
    true ->
      is_last_of_bells(Card, Hand);
    false ->
      false
  end;

valid_bella(_Card, _Hand, _Led, _TrumpSuit, _HighTrumpValue) ->
  false.

is_last_of_bells(#card{value=?tarabish_KING} = Card, Hand) ->
  OtherBella = Card#card{value=?tarabish_QUEEN},
  not(lists:member(OtherBella, Hand));

is_last_of_bells(#card{value=?tarabish_QUEEN} = Card, Hand) ->
  OtherBella = Card#card{value=?tarabish_KING},
  not(lists:member(OtherBella, Hand));

is_last_of_bells(_OtherCard, _Hand) ->
  false.

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

is_last_of_bells_test_() ->
  King = card(?tarabish_KING, ?tarabish_CLUBS),
  Queen = card(?tarabish_QUEEN, ?tarabish_CLUBS),
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

    ?_assertEqual(true, is_last_of_bells(King, [King] ++ Hand)),
    ?_assertEqual(false, is_last_of_bells(King, [King, Queen] ++ Hand)),
    ?_assertEqual(false, is_last_of_bells(King, Hand ++ [King, Queen])),

    ?_assertEqual(true, is_last_of_bells(Queen, [Queen] ++ Hand)),
    ?_assertEqual(false, is_last_of_bells(Queen, [King, Queen] ++ Hand)),
    ?_assertEqual(false, is_last_of_bells(Queen, Hand ++ [King, Queen]))
  ].
