#!/usr/bin/env python -i

# Run with ./client-py/test-manual.py
# Make calls on client object. eg: client.joinTable(1)

import sys
sys.path.append('api/target/gen-py')

import random
import thread
import string

from tarabish.thrift import Tarabish
from tarabish.thrift.ttypes import *
from tarabish.thrift.constants import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from event import print_event

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

print "Login"
cookie = client.login(name, "password")

print "Checking Table list - ",
tables = client.getTables()
print str(tables)

def join_event(cookie):
    print "Joining Event Stream, ",
    t2 = TSocket.TSocket('localhost', 42745)
    t2 = TTransport.TBufferedTransport(t2)
    p2 = TBinaryProtocol.TBinaryProtocol(t2)
    t2.open()
    event_client = Tarabish.Client(p2)
    print str(event_client.login(cookie))
    return event_client

def event_loop(cookie):
    ec = join_event(cookie)

    while True:
        try:
            events = ec.getEventsTimeout(300000)
            for event in events:
                print_event(event, -1)
        except InvalidOperation:
            print "Bad getEventsTimeout call"
            return 0

thread.start_new_thread(event_loop, (cookie,))

args = sys.argv[1:]
for arg in args:
    if arg == "-j":
        print "Joining first table"
        for i in range(4):
            try:
                client.sit(1, i)
                break
            except InvalidOperation:
                continue
