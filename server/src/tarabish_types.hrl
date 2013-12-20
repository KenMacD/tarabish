-ifndef(_tarabish_types_included).
-define(_tarabish_types_included, yeah).

-define(tarabish_HIDDEN, 0).

-define(tarabish_NONE, 0).

-define(tarabish_JACK, 11).
-define(tarabish_QUEEN, 12).
-define(tarabish_KING, 13).
-define(tarabish_ACE, 14).

-define(tarabish_PASS, 0).
-define(tarabish_CLUBS, 1).
-define(tarabish_DIAMONDS, 2).
-define(tarabish_SPADES, 3).
-define(tarabish_HEARTS, 4).

-define(tarabish_BaitType_NONE, 0).
-define(tarabish_BaitType_HALF, 1).
-define(tarabish_BaitType_FULL, 2).

-define(tarabish_RunType_NONE, 0).
-define(tarabish_RunType_TWENTY, 1).
-define(tarabish_RunType_FIFTY, 2).

-define(tarabish_BetterType_EQUAL, 0).
-define(tarabish_BetterType_BETTER, 1).

-record(card, {value :: integer(),
               suit :: integer()}).

-record(seatView, {isOpen :: boolean(),
                   name :: string() | binary()}).

-record(tableView, {tableId :: integer(),
                    seats :: list(),
                    observers :: list()}).

-endif.
