#!/usr/bin/env python -i

# Run with ./client-py/test-manual.py
# Make calls on client object. eg: client.joinTable(1)

import sys
sys.path.append('api/target/gen-py')

import random
import thread
import string

from tarabish.thrift import Tarabish, TarabishMsg
from tarabish.thrift.ttypes import *

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

name = ''.join(random.choice(string.ascii_letters) for x in range(5))


print "Creating account %s "%(name,),
try:
        print str(client.createAccount(name, "%s@invalid"%(name,), "password")),
except InvalidOperation as invalid:
        print "Didn't work, perhaps the account exists"

print "Login"
cookie = client.login(name, "password")

print "Checking Table list - ",
tables = client.getTables()
print str(tables)

def join_event(cookie):
    print "Joining Event Stream, ",
    t2 = TSocket.TSocket('localhost', 42746)
    t2 = TTransport.TBufferedTransport(t2)
    p2 = TBinaryProtocol.TBinaryProtocol(t2)
    t2.open()
    event_client = TarabishMsg.Client(p2)
    print str(event_client.login(cookie))
    return event_client

def event_loop(cookie):
    ec = join_event(cookie)

    while True:
        try:
            events = ec.getEventsTimeout(300000)
            print "Event: " + str(events)
        except InvalidOperation:
            print "Bad getEventsTimeout call"
            return 0

thread.start_new_thread(event_loop, (cookie,))
