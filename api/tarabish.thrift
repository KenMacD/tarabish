#!/usr/bin/env thrift --gen java:beans --gen py:new_style

const i32	PROTOCOL_VERSION = 1

exception InvalidOperation {
	1: string why
}

####################
# Card values:
####################
typedef byte CardSuit
typedef byte CardValue

const byte HIDDEN = 0
const byte PASS   = 0
const byte NONE	  = 0

const byte JACK   = 11
const byte QUEEN  = 12
const byte KING   = 13
const byte ACE    = 14

const byte SPADES   = 1
const byte HEARTS   = 2
const byte DIAMONDS = 3
const byte CLUBS    = 4

struct Card {
  1: byte value,
  2: byte suit,
}


struct SeatView {
  1: bool isOpen
  2: string name  # Seated clients name
}

struct TableView {
  1: i32 tableId,

  # list of 4 elements.
  2: list<SeatView> seats,

  3: list<string> observers
}

enum BaitType {
  NONE,
  HALF,
  FULL,
}

# Thrift doesn't allow different types, so only some fields populated.
# TODO: need commands for PART/STAND
enum EventType {
  JOIN,		# table, name
  PART,		# table, name
  SIT,		# table, name, seat
  STAND,	# table, name, seat

  CHAT,		# table, message TODO: update to contain name?

  NEW_GAME,	# table

  DEALER,	# table, seat (0-3)
  DEAL,		# table, seat=first-dealt, dealt (to you)
  ASK_TRUMP,	# table, seat
  CALL_TRUMP,	# table, seat, suit

  ASK_CARD,	# table, seat
  PLAY_CARD,	# table, seat, card

  TAKE_TRICK,	# table, seat
  HAND_DONE,	# table, hand_score, score, bait

  GAME_DONE,	# table, score, seat(0,1)=winner

}

struct Event {
  1: EventType	type,
  2: i32	table,

  3: string	name,

  4: string	message,

  5: byte	seat,

  6: list<Card> dealt,

  7: byte	suit,

  8: Card	card,

  9: list<i32>  hand_score,
 10: list<i32>  score,
 11: BaitType	bait,
}

service Tarabish
{
	# Always works and returns protocol version.
	i32 getVersion()

	void createAccount(1: string name, 2: string email, 3: string password)
		throws (1:InvalidOperation invalid)

	# Returns a cookie to use for the message side.
	i64 login(1: string name, 2: string password)
		throws (1:InvalidOperation invalid)


	##### After login #####
	void joinTable(1: i32 table_id)
		throws (1:InvalidOperation invalid)

	list<TableView> getTables()
		throws (1:InvalidOperation invalid)

	void sit(1: i32 table_id, 2: byte seat)
		throws (1:InvalidOperation invalid)

	void stand(1: i32 table_id)
		throws (1:InvalidOperation invalid)

	void partTable(1: i32 table_id)
		throws (1:InvalidOperation invalid)

	void quit()
		throws (1:InvalidOperation invalid)

	##### After joining a table #####
	void chat(1: i32 table, 2: string message)
		throws (1:InvalidOperation invalid)

	##### Once we have a full table #####
	void startGame(1: i32 table_id)
		throws (1:InvalidOperation invalid)

	void callTrump(1: i32 table_id, 2: byte suit)
		throws (1:InvalidOperation invalid)

	void playCard(1: i32 table_id, 2: Card card)
		throws (1:InvalidOperation invalid)
}

service TarabishMsg
{
	i32 getVersion()

	void login(1: i64 cookie)
		throws (1:InvalidOperation invalid)

	list<Event> getEvents()
		throws (1:InvalidOperation invalid)

	list<Event> getEventsTimeout(1: i32 timeout_mills)
		throws (1:InvalidOperation invalid)
}
