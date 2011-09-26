#!/usr/bin/env python

# Basic bot to join a table and call trump.

import sys
import random
import string
from time import sleep
sys.path.append('api/target/gen-py')

from tarabish.thrift import Tarabish, TarabishMsg
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
tables = client.getTables()
print str(tables)

tablenum = 0
def join_first_seat(tables):
    for table in tables:
        seat_num = 0
        for seat in table.seats:
            if seat.isOpen:
                print "Found open seat"
                client.sit(table.tableId, seat_num)
                if seat_num == 3:
                    print "Starting the game"
                    global tablenum
                    tablenum = table.tableId
                    client.startGame(table.tableId)
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

game = 0
ec = join_event(cookie)
cards = []
trick = 0
#count = 0
while True:
    events = ec.getEventsTimeout(300000)
    for event in events:
        print_event(event, seatnum)
#        if event.type == EventType.HAND_DONE:
#            count = count + 1
#            print "BELLA hand count: " + str(count)
        if event.type == EventType.DEAL:
            cards += event.dealt
            trick = 0
        if event.type == EventType.ASK_TRUMP and event.seat == seatnum:
            try:
                client.callTrump(tableid, PASS)
            except InvalidOperation, e:
                # Forced
                client.callTrump(tableid, SPADES)
        if event.type == EventType.PLAY_CARD and event.seat == seatnum:
             cards.remove(event.card)
        if event.type == EventType.ASK_CARD and event.seat == seatnum:
            if trick == 0:
                try:
                    client.callRun(tableid)
                    print "Called Run!"
                except InvalidOperation, e:
                    pass # expected
            if trick == 1:
                try:
                    client.showRun(tableid)
                    print "Showed Run!"
                except InvalidOperation, e:
                    pass # expected
            played = 0
            # try playing each card a bells first:
            try:
                client.playBella(tableid)
                print "Played Bella!"
                played = 1
            except InvalidOperation, e:
                for card in cards[:]:
                    try:
                        client.playCard(tableid, card)
                        played = 1
                        break;
                    except InvalidOperation, e:
                        pass # expected

            if not played:
                print "!!! No valid cards in hand: " + str(cards)
            trick = trick + 1
        if event.type == EventType.GAME_DONE:
            game = game + 1
            if game < 5 and seatnum == 3:
                client.startGame(tablenum)
            elif game == 5:
                sys.exit(1)


sys.exit(1)
