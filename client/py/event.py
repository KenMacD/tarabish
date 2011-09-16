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

def print_event(event, seat=None):
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
