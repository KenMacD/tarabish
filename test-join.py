#!/usr/bin/env python

import sys
sys.path.append('api/target/gen-py')

from tarabish import Tarabish, TarabishMsg
from tarabish.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

# Make socket
transport = TSocket.TSocket('localhost', 42745)

# Buffering is critical. Raw sockets are very slow
transport = TTransport.TBufferedTransport(transport)

# Wrap in a protocol
protocol = TBinaryProtocol.TBinaryProtocol(transport)

# Create a client to use the protocol encoder
client = Tarabish.Client(protocol)

# Connect!
transport.open()
 
print "Get Version - " + str(client.getVersion())

print "Creating Bob - ",
try:
    print str(client.createAccount("Bob", "b@invalid", "Password"))
except InvalidOperation as invalid:
    print str(invalid)

print "Invalid Login - ",
try:
    client.login("Bob", "PasswordWrong")
    print "FAILED"
except InvalidOperation as invalid:
    print str(invalid)

print "Valid Login - ",
cookie = client.login("Bob", "Password")
print str(cookie)

print "Checking Table list - ",
tables = client.get_tables()
print str(tables)

table = 1
client.join_table(table)
print "Sending message to table " + str(table) + " - ",
try:
    print client.chat(table, "Test Message From Bob")
except InvalidOperation as invalid:
    print str(invalid)

print "Checking Table list - ",
tables = client.get_tables()
print str(tables)

raw_input("Press Enter to continue...")

t2 = TSocket.TSocket('localhost', 42746)
t2 = TTransport.TBufferedTransport(t2)
p2 = TBinaryProtocol.TBinaryProtocol(t2)
c2 = TarabishMsg.Client(p2)
t2.open()

print "In MSG"

print "Get Version - " + str(c2.getVersion())

print "Invalid Login - ",
try:
    c2.login(12345)
    print "FAILED"
except InvalidOperation as invalid:
    print str(invalid)

print "Valid Login - ",
print str(c2.login(cookie))

print "Getting Events -",
print str(c2.get_events())
