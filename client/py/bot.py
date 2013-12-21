#!/usr/bin/env python

# Basic bot to join a table and call trump.

import sys
import json
import random

import websocket

myseat = -1
cards = []

def tome(event):
    if 'seat' in event and event['seat'] == myseat:
        return True
    return False

def wssend(ws, method, **kwargs):
    print "Sending method %s" %(method,)
    try:
        message = {"method": method}
        message.update(kwargs)
        jmsg = json.dumps(message)
        ws.send(jmsg)
    except Exception as e:
        print "Error sending: " + str(e)

def on_open(ws):
    print "On open called"
    name = "User%d" %(random.randint(1000, 9999))
    print "Name: " + name
    wssend(ws, "login", name=name)

def on_message(ws, message):
    global cards
    print("< " + message)
    try:
        event = json.loads(message)
    except:
        print "Load failed"
        return

    mtype = event['type']
    print "Type: %s" %(mtype,)

    if mtype == "valid_login":
        wssend(ws, "get_tables")
    elif mtype == "tables":
        tables = event['tables']
        for table in tables:
            tid = table['tableId']
            # Only join table 1 so far
            if tid != 1:
                return
            for seat in table['seats']:
                if seat['isOpen']:
                    global myseat
                    myseat = seat["num"]
                    wssend(ws, "sit", table_id=tid, seat=seat["num"])
                    return
    elif mtype == "deal":
        for card in event['dealt']:
            suit = card['suit']
            value = card['value']
            cards.append((value, suit))
    elif mtype == "play_card" and tome(event):
        card = event["card"]
        value = card["value"]
        suit = card["suit"]
        cards.remove((value, suit))
    elif mtype == "ask_trump" and tome(event):
        wssend(ws, "call_trump", table_id=1, suit=1)
    elif mtype == "game_cancel":
        cards = []
    elif mtype == "ask_card" and tome(event):
        for (val, suit) in cards:
            wssend(ws, "play_card", table_id=1, card = {"value":val, "suit":suit})


ws = websocket.WebSocketApp("ws://127.0.0.1:42745/websocket",
        on_message = on_message)
ws.on_open = on_open
ws.run_forever()
sys.exit(1)
