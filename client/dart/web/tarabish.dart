import 'dart:html';
import 'dart:async';
import 'package:web_ui/web_ui.dart';

// initial value for click-counter
int startingCount = 5;

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
  }

}

/**
 * Learn about the Web UI package by visiting
 * http://www.dartlang.org/articles/dart-web-components/.
 */
void main() {
  // Enable this to use Shadow DOM in the browser.
  //useShadowDom = true;
  var server = new TarabishSrv("ws://localhost:42745/websocket");
}
