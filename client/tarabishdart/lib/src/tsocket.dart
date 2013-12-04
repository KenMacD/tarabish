library tsocket;

import 'dart:html';
import 'dart:async';
import 'dart:collection';
import 'dart:convert';

import 'model.dart';

typedef void validLoginFun(String name);
typedef void lobbyUpdateFun(List<TableView> tables);

mkmsg(String method, [Map others = null]) {
  var message = {"method": method};
  if (others != null) {
    message.addAll(others);
  }
  return message;
}

suit_toString(suit) {
  return ["clubs", "diamonds", "spades", "hearts"][suit - 1];
}

abstract class TarabishCallbacks {
  void validLogin(String name);
  void lobbyUpdate(List<TableView> tables);

  void youSat(Table table);
}

typedef void updatedCallback();

/* A connection to the back end, it could be made more generic */
class TarabishSocket {
  // TODO: add logged_in
  String url;
  WebSocket webSocket;
  bool _connected = false;
  var waiting_msgs = new Queue<String>();

  validLoginFun onValidLogin;
  lobbyUpdateFun onLobbyUpdate;
  TarabishCallbacks callbacks;

  // For now only 1 table:
  Table table;

  TarabishSocket(this.url, this.callbacks) {
    init();
  }

  init([int retrySeconds = 2]) {
    if (_connected) {
      return;
    }
    bool reconnectScheduled = false;
    webSocket = new WebSocket(url);

    scheduleReconnect() {
      if (!reconnectScheduled) {
        reconnectScheduled = true;
        _connected = false;
        print('web socket closed, retrying in $retrySeconds seconds');
        new Timer(new Duration(seconds: retrySeconds),
            () => init(retrySeconds * 2));
      }
    }

    webSocket.onOpen.listen((e) {
      print("Connected with ${waiting_msgs.length} messages to send");
      retrySeconds = 2;
      _connected = true;

      // Send waiting messages:
      for (var message in waiting_msgs) {
        webSocket.send(message);
        // TODO: handle only a few sent?
      }
      waiting_msgs.clear();
    });

    webSocket.onClose.listen((e) => scheduleReconnect());
    webSocket.onError.listen((e) => scheduleReconnect());
    webSocket.onMessage.listen((e) => _receiveEvent(e.data));
  }

  _send(String data) {
    if (_connected) {
      webSocket.send(data);
    } else {
      waiting_msgs.add(data);
    }
  }

  _receiveEvent(String encodedMessage) {
    Map message;
    try {
      message = JSON.decode(encodedMessage);
    } on FormatException {
      print("Invalid message $encodedMessage");
      return;
    }
    if (!message.containsKey('type')) {
      print("Invalid message, no type: $encodedMessage");
      return;
    }
    switch (message['type']) {
      case "tables":
        List<TableView> tables = new List();
        print ("Received tables message, parsing");
        for (var table in message['tables']) {
          tables.add(new TableView.from_json(table));
        }
        // TODO: create lobby.
        callbacks.lobbyUpdate(tables);
        break;
      case "valid_login":
        callbacks.validLogin(message['name']);
        break;
      case "table_view_sit":
        var id = message['tableId'];
        var seat = message['seat'];
        var view = new TableView.from_json(message['table_view']);
        this.table = new Table(id, view, seat);
        callbacks.youSat(this.table);
        break;
      case "ask_trump":
        table.recv_ask_trump(message['seat']);
        break;
      case "ask_card":
        table.recv_ask_card(message['seat']);
        break;
      case "trump_passed":
        table.recv_trump_passed(message['seat']);
        break;
      case "trump_called":
        table.recv_trump_called(message['seat'], message['suit']);
        break;
      case "chat":
        var chat_msg = message['message'];
        var chat_name = message['name'];
        table.recvChat(chat_name, chat_msg);
        break;
      case "sit":
        table.recvSit(message['seat'], message['name']);
        break;
      case "part":
        table.recvPart(message['seat'],  message['name']);
        break;
      case "game_cancel":
        // TODO: current this message can be sent after a part message. fix
        if (table != null) {
          table.recv_game_cancel();
        }
        break;
      case "deal":
        var dealer = message['dealer'];
        List<Card> cards = new List();
        for (var card in message['dealt']) {
          cards.add(new Card.from_json(card));
        }
        table.recv_deal(dealer, cards);
        break;
      case "play_card":
        table.recv_play_card(message['seat'], message['card']);
        break;
      case "take_trick":
        table.recv_take_trick(message['seat']);
        break;
      case "call_run":
        table.recv_call_run(message['seat'], message['run']);
        break;
      case "show_run":
        List<Card> cards = new List();
        for (var card in message['cards']) {
          cards.add(new Card.from_json(card));
        }
        table.recv_show_run(message['seat'], message['run'], cards);
        break;
      case "noshow_run":
        table.recv_noshow_run(message['seat'], message['better'], message['run'],
            message['high_value'], message['is_trump'], message['other_seat']);
        break;
      case "call_bella":
        table.recv_call_bella(message['seat']);
        break;
        var type = message['type'];
        print("Received message with type $type");
        print("Message: $message");
        // TODO: handle
    }
  }

  void login(String name) {
    var login = mkmsg("login", {"name": name});
    _send(JSON.encode(login));
  }

  void getTables() {
    _send(JSON.encode(mkmsg("get_tables")));
  }

  void sit(int table, int seat) {
    var sit = mkmsg("sit", {
      "table_id": table,
      "seat": seat
    });
    _send(JSON.encode(sit));
  }

  void startGame(int table) {
    var start = mkmsg("start_game", {"table_id": table});
    _send(JSON.encode(start));
  }

  void sendChat(int table, String message) {
    var chat = mkmsg("chat", {"table_id": table, "message": message});
    _send(JSON.encode(chat));
  }
}