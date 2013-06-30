import 'dart:html';
import 'dart:async';
import 'dart:collection';
import 'dart:json' as json;
import 'package:web_ui/web_ui.dart';

typedef void MessageCallback(String data);

class TarabishSocket {
  // TODO: add logged_in
  String url;
  WebSocket webSocket;
  bool _connected = false;
  int cookie;
  Map<String, dynamic> eventMap;
  var waiting_msgs = new Queue<String>();

  TarabishSocket(this.url) {
    eventMap = new Map();
    _init();
  }

  _init([int retrySeconds = 2]) {
    bool reconnectScheduled = false;
    webSocket = new WebSocket(url);

    scheduleReconnect() {
      if (!reconnectScheduled) {
        reconnectScheduled = true;
        _connected = false;
        print('web socket closed, retrying in $retrySeconds seconds');
        new Timer(new Duration(seconds: retrySeconds),
            () => _init(retrySeconds * 2));
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
    if (message['type'] != null) {
      var type = message['type'];
      print("Received message with type $type");
      // TODO: handle
    }
  }
}

@observable
class Tarabish {
  TarabishSocket _tsocket;

  bool loggedin = false;
  String loginName = "Nobody";

  Tarabish();

  // Lazy start socket on first login
  _setup_socket() {
    if (_tsocket == null) {
      _tsocket = new TarabishSocket("ws://localhost:42745/websocket");
    }
  }

  do_login(Event e) {
    e.preventDefault();
    _setup_socket();

    InputElement loginNameElement = query("#login-name");
    var login = {
                 "method": "login",
                 "name": loginNameElement.value
    };
    _tsocket.send(json.stringify(login));
    print("Login called");
  }
  refresh_tables(Event e) {
    e.preventDefault();
    var table_req = {"method": "get_tables"};
    _tsocket.send(json.stringify(table_req));
  }
}
Tarabish tarabish;


/**
 * Learn about the Web UI package by visiting
 * http://www.dartlang.org/articles/dart-web-components/.
 */
void main() {
  // Enable this to use Shadow DOM in the browser.
  //useShadowDom = true;
  tarabish = new Tarabish();

}
