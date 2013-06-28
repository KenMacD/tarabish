import 'dart:html';
import 'dart:async';
import 'dart:json' as json;
import 'package:web_ui/web_ui.dart';


class TarabishSrv {
  String url;
  WebSocket webSocket;
  int cookie;

  TarabishSrv(this.url) {
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
    webSocket.onMessage.listen((MessageEvent e) => _receiveEvent(e.data));
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
String loginName = "Nobody";


void do_login(Event e) {
  e.preventDefault();
  InputElement loginNameElement = query("#login-name");
  loginName = loginNameElement.value;
  print("Login called");
}


/**
 * Learn about the Web UI package by visiting
 * http://www.dartlang.org/articles/dart-web-components/.
 */
void main() {
  // Enable this to use Shadow DOM in the browser.
  //useShadowDom = true;
  TarabishSrv server = new TarabishSrv("ws://localhost:42745/websocket");



}
