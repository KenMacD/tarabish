import 'dart:html';
import 'dart:async';
import 'dart:json' as json;
import 'package:web_ui/web_ui.dart';

typedef void MessageCallback(String data);

class ReconnectingSocket {
  WebSocket webSocket;
  String url;

  ReconnectingSocket(this.url) {
    _init();
  }

  _init([int retrySeconds = 2]) {
    bool reconnectScheduled = false;
    webSocket = new WebSocket(url);

    scheduleReconnect() {
      if (!reconnectScheduled) {
        print('web socket closed, retrying in $retrySeconds seconds');
        new Timer(new Duration(seconds: retrySeconds),
            () => _init(retrySeconds * 2));
      }
      reconnectScheduled = true;
    }

    webSocket.onOpen.listen((e) {
      print('Connected');
      retrySeconds = 2;
    });

    webSocket.onClose.listen((e) => scheduleReconnect());
    webSocket.onError.listen((e) => scheduleReconnect());
  }

  send(String data) {
    webSocket.send(data);
  }

  // There's probably a better way to proxy these, I don't know it.
  Stream<Event> get onOpen => webSocket.onOpen;
  Stream<CloseEvent> get onClose => webSocket.onClose;
  Stream<Event> get onError => webSocket.onError;
  Stream<MessageEvent> get onMessage => webSocket.onMessage;

}

class TarabishSocket {
  // TODO: add logged_in, and queue for messages while not logged_in
  ReconnectingSocket webSocket;
  int cookie;
  Map<String, dynamic> eventMap;

  TarabishSocket(url) {
    eventMap = new Map();
    webSocket = new ReconnectingSocket(url);
    webSocket.onMessage.listen((e) => _receiveEvent(e.data));
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

  _login(String name) {
    var login = {
                 "method": "login",
                 "name": name
    };
    webSocket.send(json.stringify(login));
  }
}
TarabishSocket tserver;

@observable
String loginName = "Nobody";

void do_login(Event e) {
  e.preventDefault();
  InputElement loginNameElement = query("#login-name");
  tserver._login(loginNameElement.value);
  print("Login called");
}


/**
 * Learn about the Web UI package by visiting
 * http://www.dartlang.org/articles/dart-web-components/.
 */
void main() {
  // Enable this to use Shadow DOM in the browser.
  //useShadowDom = true;
  tserver = new TarabishSocket("ws://localhost:42745/websocket");

}
