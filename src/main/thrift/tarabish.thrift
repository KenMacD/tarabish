#!/usr/bin/env thrift --gen java:beans --gen py:new_style

const i32	PROTOCOL_VERSION = 1

service Tarabish
{
	i32 getVersion();

	bool createAccount(1: string name, 2: string email, 3: string password);
}
