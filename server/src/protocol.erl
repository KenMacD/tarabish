-module(protocol).

-include("tarabish_types.hrl").

-export([deseralize_card/1, seralize_cards/1, seralize_card/1, seralize_hands/1]).

deseralize_card(Card) ->
  Value = proplists:get_value(value, Card),
  Suit = proplists:get_value(suit, Card),
  #card{value=Value, suit=Suit}.

seralize_card(#card{value=Value, suit=Suit}) ->
  [{value, Value}, {suit, Suit}].

seralize_cards(Cards) ->
  lists:map(fun seralize_card/1, Cards).

seralize_hands(Hands) ->
  Hand = fun(H) -> lists:map(fun seralize_card/1, H) end,
  lists:map(Hand, Hands).
