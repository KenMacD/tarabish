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

def print_event(event, seat):
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

    def format_ask_card(event, seat):
        if seat == event.seat:
            return "Ask Card: You are requested to play a card at %d"%(event.table)
        else:
            return "Ask Card: %d is requested to play a card at %d"%(event.seat,
                    event.table)

    format = {
            EventType.JOIN: lambda e: "Join: %s joined table %d"%(e.name,
                e.table),
            EventType.SIT: lambda e: "Sit: %s sat in seat %d at table %d"%
                (e.name, e.seat, e.table),
            EventType.NEW_GAME: lambda e: "New Game: at table %d"%(e.table),
            EventType.DEALER: lambda e: "Dealer: is %d at table %d"%(e.seat,
                e.table),
            EventType.DEAL: lambda e: "Deal: You received card %s at %d"%(
                event.dealt, event.table),
            EventType.ASK_TRUMP: lambda e: format_ask_trump(e, seat),
            EventType.CALL_TRUMP: lambda e: format_call_trump(e, seat),
            EventType.ASK_CARD: lambda e: format_ask_card(e, seat),
            EventType.PLAY_CARD: lambda e: "Card: %d played %s at %d"%(
                event.seat, str(event.card), event.table),
            EventType.CALL_BELLA: lambda e: "Bella: %d called bella at %d"%(
                event.seat, event.table),
            EventType.CALL_RUN: lambda e: "Run: %d called run type %d"%(
                event.seat, event.run),
            EventType.SHOW_RUN: lambda e: "Show: %d showed run"%(event.seat),
            EventType.NOSHOW_RUN: lambda e: "Show: %d couldn't show run"%(event.seat),
            EventType.TAKE_TRICK: lambda e: "Trick: %d took trick at %d"%(
                event.seat, event.table),
            EventType.HAND_DONE: lambda e: \
                "Hand: Scores %d, %d Total %d %d, Bait %d"%(
                event.hand_score[0], event.hand_score[1],
                event.score[0], event.score[1], event.bait),
            EventType.GAME_DONE: lambda e: \
                "Game Over: %d wins at table %d"%(event.seat, event.table)
            }
    if event.type in format:
        print format[event.type](event)
    else:
        print "Unknown Event %s"%(str(event))

game = 0
ec = join_event(cookie)
cards = []
trick = 0
while True:
    events = ec.getEventsTimeout(300000)
    for event in events:
        print_event(event, seatnum)
        if event.type == EventType.DEAL:
            cards += event.dealt
            trick = 0
        if event.type == EventType.ASK_TRUMP and event.seat == seatnum:
            try:
                client.callTrump(tableid, PASS)
            except InvalidOperation, e:
                # Forced
                client.callTrump(tableid, SPADES)
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
            for card in cards[:]:
                try:
                    client.playBella(tableid, card)
                    cards.remove(card)
                    print "Played Bella!"
                    played = 1
                    break;
                except InvalidOperation, e:
                    try:
                        client.playCard(tableid, card)
                        cards.remove(card)
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