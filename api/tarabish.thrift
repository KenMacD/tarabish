#!/usr/bin/env thrift --gen java:beans --gen py:new_style

const i32	PROTOCOL_VERSION = 1

exception InvalidOperation {
	1: string why
}

enum EventType {
  CHAT = 1,
}

struct Event {
  1: EventType	type,
  2: i32	table,
  3: string	message,
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

	# Returns table_id.
	i32 create_table()
		throws (1:InvalidOperation invalid)

	void chat(1: i32 table, 2: string message)
		throws (1:InvalidOperation invalid)
}

service TarabishMsg
{
	i32 getVersion()

	void login(1: i64 cookie)
		throws (1:InvalidOperation invalid)

	list<Event> get_events()
		throws (1:InvalidOperation invalid)
}
