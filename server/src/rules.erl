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
-export([valid_play/5, valid_bella/4]).

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
valid_bella(Hand, Led, TrumpSuit, HighTrumpValue) ->
  case has_last_of_bells(Hand, TrumpSuit) of
    {true, Card} ->
      case valid_play(Card, Hand, Led, TrumpSuit, HighTrumpValue) of
        true -> {true, Card};
        false -> false
      end;
    false ->
      false
  end.

has_last_of_bells(Hand, TrumpSuit) ->
  has_last_of_bells(Hand, TrumpSuit, none, 0).

has_last_of_bells([], _Suit, Bell, 1) ->
  {true, Bell};

has_last_of_bells([], _Suit, _Bell, _Other) ->
  false;

has_last_of_bells([#card{value=?tarabish_KING, suit=Suit} = Card | Rest],
    Suit, _Bell, Count) ->
  has_last_of_bells(Rest, Suit, Card, Count + 1);

has_last_of_bells([#card{value=?tarabish_QUEEN, suit=Suit} = Card | Rest],
    Suit, _Bell, Count) ->
  has_last_of_bells(Rest, Suit, Card, Count + 1);

has_last_of_bells([_Other|Rest], Suit, Bell, Count) ->
  has_last_of_bells(Rest, Suit, Bell, Count).

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

has_last_of_bells_test_() ->
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

    ?_assertEqual({true, King}, has_last_of_bells([King] ++ Hand,
      ?tarabish_CLUBS)),
    ?_assertEqual(false, has_last_of_bells([King, Queen] ++ Hand,
      ?tarabish_CLUBS)),
    ?_assertEqual(false, has_last_of_bells(Hand ++ [King, Queen],
      ?tarabish_CLUBS)),

    ?_assertEqual({true, Queen}, has_last_of_bells([Queen] ++ Hand,
      ?tarabish_CLUBS)),
    ?_assertEqual(false, has_last_of_bells([King, Queen] ++ Hand,
      ?tarabish_CLUBS)),
    ?_assertEqual(false, has_last_of_bells(Hand ++ [King, Queen],
        ?tarabish_CLUBS))
  ].
