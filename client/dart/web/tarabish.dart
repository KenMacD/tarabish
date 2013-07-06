import 'dart:html';
import 'dart:async';
import 'dart:collection';
import 'dart:json' as json;
import 'package:web_ui/web_ui.dart';

typedef void MessageCallback(dynamic data);

// Global state
TarabishSocket tsocket;

Tarabish tarabish;

@observable
Table table;


class TarabishSocket {
  // TODO: add logged_in
  String url;
  WebSocket webSocket;
  bool _connected = false;
  Map<String, List<MessageCallback>> eventMap;
  var waiting_msgs = new Queue<String>();

  TarabishSocket(this.url) {
    eventMap = new Map();
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

  send(String data) {
    if (_connected) {
      webSocket.send(data);
    } else {
      waiting_msgs.add(data);
    }
  }

  _receiveEvent(String encodedMessage) {
    Map message;
    try {
      message = json.parse(encodedMessage);
    } on FormatException {
      print("Invalid message $encodedMessage");
      return;
    }
    if (message['type'] == "tables") {
      List<TableView> tables = new List();
      print ("Received tables message, parsing");
      for (var table in message['tables']) {
        tables.add(new TableView.from_json(table));
      }
      // TODO: create lobby.
      tarabish.update_lobby(tables);
    } else if (message['type'] == "valid_login") {
      tarabish.valid_login(message['name']);
    } else if (message['type'] == "table_view") {
      var view = new TableView.from_json(message['table_view']);
      var id = message['tableId'];
      table = new Table(id, view);
    } else if (message['type'] == "chat") {
      var chat_msg = message['message'];
      var chat_name = message['name'];
      table.recv_chat(chat_name, chat_msg);
    } else if (message['type'] != null) {
      var type = message['type'];
      print("Received message with type $type");
      print("Message: $message");
      // TODO: handle
    }
  }

  subscribe(String messageType, MessageCallback callback) {
    // TODO: if types are predefined then create the map structure once
    if (!eventMap.containsKey(messageType)) {
      eventMap[messageType] = new List();
    }
    eventMap[messageType].add(callback);
  }
}

class SeatView {
  bool isOpen;
  String name;
  int num;

  SeatView(this.isOpen, this.name, this.num);

  factory SeatView.from_json(json) {
    return new SeatView(json['isOpen'], json['name'], json['num']);
  }

  String toString() {
    if (isOpen) return "Seat open";
    else return "Seat occupied by $name";
  }
}

class TableView {
  int tableId;
  List<SeatView> seats;

  TableView(this.tableId, this.seats);

  factory TableView.from_json(json) {
    var seats = new List<SeatView>();
    for (var seat in json['seats']) {
      seats.add(new SeatView.from_json(seat));
    }
    return new TableView(json['tableId'], seats);
  }

  String toString() {
    String view = "Table $tableId -- ";
    for (var seat in seats) {
      view += "[ $seat ] ";
    }
    return view;
  }
}

@observable
class Table {
  int id;
  TableView view;

  Table(this.id, this.view);

  chat(Event e) {
    e.preventDefault();

    // TODO: is there a better way to get these values?
    InputElement chat_msg_elm = query("#chat-msg");
    var chat = mkmsg("chat", {"table_id": id, "message": chat_msg_elm.value});
    chat_msg_elm.value = "";
    tsocket.send(json.stringify(chat));
  }

  recv_chat(name, message) {
    var output = query('#chat-display');
    var text = "$name: $message";
    if (!output.text.isEmpty) {
      text = "${text}\n${output.text}";
    }
    output.text = text;
  }
}

@observable
class Tarabish {
  bool loggedin = false;
  String loginName = "Nobody";

  List<TableView> tableViews;

  Tarabish();

  // Lazy start socket on first login
  _setup_socket() {
    tsocket.init();
  }

  valid_login(name) {
    loggedin = true;
    loginName = name;
  }

  update_lobby(tables) {
    tableViews = tables;
  }

  // Temporary disconnect to test re-attach
  do_disconnect(Event e) {
    e.preventDefault();
    tsocket.webSocket.close();
  }

  do_login(Event e) {
    e.preventDefault();
    _setup_socket();

    InputElement loginNameElement = query("#login-name");
    var login = mkmsg("login", {"name": loginNameElement.value});
    tsocket.send(json.stringify(login));
    print("Login called");
  }

  // TODO: add a @require_socket
  refresh_tables(Event e) {
    e.preventDefault();
    tsocket.send(json.stringify(mkmsg("get_tables")));
  }

  sit(table, seat) {
    var sit = mkmsg("sit", {
      "table_id": table,
      "seat": seat
    });
    tsocket.send(json.stringify(sit));
    print("Sit called $table -- $seat");
  }
}

mkmsg(String method, [Map others = null]) {
  var message = {"method": method};
  if (others != null) {
    message.addAll(others);
  }
  return message;
}

/**
 * Learn about the Web UI package by visiting
 * http://www.dartlang.org/articles/dart-web-components/.
 */
void main() {

  // Enable this to use Shadow DOM in the browser.
  //useShadowDom = true;
  tsocket = new TarabishSocket("ws://127.0.0.1:42745/websocket");
  tarabish = new Tarabish();

  query('#disconnect').onClick.listen((e) => tarabish.do_disconnect(e));
}
