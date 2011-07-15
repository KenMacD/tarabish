#!/usr/bin/env python

# Basic bot to join a table and call trump.

import sys
import random
import string
from time import sleep
sys.path.append('api/target/gen-py')

from tarabish import Tarabish, TarabishMsg
from tarabish.ttypes import *
from tarabish.constants import *

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

print "Get Version:" + str(client.getVersion())

name = ''.join(random.choice(string.ascii_letters) for x in range(5))

print "Creating account %s "%(name,),

try:
    print str(client.createAccount(name, "%s@invalid"%(name,), "password")),
except InvalidOperation as invalid:
    print "Didn't work, perhaps the account exists"

print "Login"
cookie = client.login(name, "password")

print "Checking Table list - ",
tables = client.get_tables()
print str(tables)

def join_first_seat(tables):
    for table in tables:
        seat_num = 0
        for seat in table.seats:
            if seat.isOpen:
                print "Found open seat"
                client.sit(table.tableId, seat_num)
                if seat_num == 3:
                    print "Starting the game"
                    client.start_game(table.tableId)
                return (table.tableId, seat_num)
            seat_num += 1

print "Joining table"
(tableid, seatnum) = join_first_seat(tables)


def join_event(cookie):
    print "Joining Event Stream, ",
    t2 = TSocket.TSocket('localhost', 42746)
    t2 = TTransport.TBufferedTransport(t2)
    p2 = TBinaryProtocol.TBinaryProtocol(t2)
    t2.open()
    event_client = TarabishMsg.Client(p2)
    print str(event_client.login(cookie))
    return event_client

def print_event(event, seat):
    def format_deal(event):
        if event.cards:
            return "Deal: You receive cards %s at table %d"%(event.cards, event.table)
        else:
            return "Deal: %d received cards at table %d"%(event.seat, event.table)

    def format_ask_trump(event,seat):
        if seat == event.seat:
            return "Ask Trump: You are requested to call trump at %d"%(event.table)
        else:
            return "Ask Trump: %d is requested to call trump at %d"%(event.seat, event.table)

    def format_call_trump(event, seat):
        if seat == event.seat:
            someone = "You"
        else:
            someone = "%d"%(event.seat)

        if event.suit == PASS:
            return "Call Trump: %s passed on trump at %d"%(someone, event.table)
        else:
            return "Call Trump: %s called %s for trump at %d"%(someone,
                    event.suit, event.table)

    format = {
            EventType.JOIN: lambda e: "Join: %s joined table %d"%(e.name,
                e.table),
            EventType.SIT: lambda e: "Sit: %s sat in seat %d at table %d"%
                (e.name, e.seat, e.table),
            EventType.NEW_GAME: lambda e: "New Game: at table %d"%(e.table),
            EventType.DEALER: lambda e: "Dealer: is %d at table %d"%(e.seat,
                e.table),
            EventType.DEAL: format_deal,
            EventType.ASK_TRUMP: lambda e: format_ask_trump(e, seat),
            EventType.CALL_TRUMP: lambda e: format_call_trump(e, seat),
            }
    if event.type in format:
        print format[event.type](event)
    else:
        print "Unknown Event %s"%(str(event))

ec = join_event(cookie)
while True:
    events = ec.get_events_timeout(300000)
    for event in events:
        print_event(event, seatnum)
        if event.type == EventType.ASK_TRUMP and event.seat == seatnum:
            if (seatnum > 0):
                client.call_trump(tableid, SPADES)
            else:
                client.call_trump(tableid, PASS)

sys.exit(1)
