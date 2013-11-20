import 'dart:html';
import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:math';

import 'package:polymer/polymer.dart';


//
//
////export 'package:polymer/init.dart';
//// import 'package:tarabish/canvas.dart';
//
//
//debug_sit() {
//  var rng = new Random();
//  var login_elm = querySelector("#login-name");
//  login_elm.value = "User" + rng.nextInt(1000).toString();
//  tarabish.do_login(new Event("fake"));
//  tarabish.refresh_tables(new Event("fake"));
//}
//
//debug_clone() {
//  var url = window.location.toString();
//  window.open(url, "_blank");
//}
//
//const int PASS = 0;
//const int CLUBS = 1;
//const int DIAMONDS = 2;
//const int SPADES = 3;
//const int HEARTS = 4;
//
//const int TWENTY = 1;
//const int FIFTY = 2;
//
typedef void MessageCallback(dynamic data);

//// Global state
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
        tarabish.update_lobby(tables);
        break;
      case "valid_login":
        tarabish.valid_login(message['name']);
        break;
      case "table_view_sit":
        var view = new TableView.from_json(message['table_view']);
        var id = message['tableId'];
        var seat = message['seat'];
        table = new Table(id, view, seat);
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
        table.recv_chat(chat_name, chat_msg);
        break;
      case "sit":
        table.recv_sit(message['seat'], message['name']);
        break;
      case "part":
        table.recv_part(message['seat'], message['name']);
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
      default:
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

@observable
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

@observable
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
  int seat; // Your seat

  List<Card> cards = toObservable(new List());
  int dealer;

  bool askTrump = false;

  Table(this.id, this.view, this.seat);

  chat(Event e) {
    e.preventDefault();

    // TODO: is there a better way to get these values?
    InputElement chat_msg_elm = querySelector("#chat-msg");
    var chat = mkmsg("chat", {"table_id": id, "message": chat_msg_elm.value});
    chat_msg_elm.value = "";
    tsocket.send(JSON.encode(chat));
  }

  recv_chat(name, message) {
    var output = querySelector('#chat-display');
    var text = "$name: $message";
    if (!output.text.isEmpty) {
      text = "${text}\n${output.text}";
    }
    output.text = text;
  }

  recv_sit(seat_num, name) {
    var seat = view.seats.elementAt(seat_num);
    seat.isOpen = false;
    seat.name = name;
    // TODO: refactor to recv_chat to accept server messages.
    recv_chat("Table", "$name sat"); // TODO: print which seat
  }

  part() {
    var part = mkmsg("part_table", {"table_id": id});
    tsocket.send(JSON.encode(part));
  }

  recv_part(seat_num, name) {
    if (seat_num == seat) {
      table = null;
    } else {
      var seat = view.seats.elementAt(seat_num);
      seat.isOpen = true;
      seat.name = null;
      recv_chat("Table", "$name left the table");
    }
  }

  new_game() {
    var start = mkmsg("start_game", {"table_id": id});
    tsocket.send(JSON.encode(start));
  }

  recv_deal(new_dealer, new_cards) {
    cards.addAll(new_cards);
    dealer = new_dealer;
    print("Received new dealer $dealer and cards $new_cards");
  }

  recv_ask_trump(seat) {
    recv_chat("Table", "Seat $seat asked to call trump");
    if (seat == this.seat) {
      askTrump = true;
    } else {
      askTrump = false;
    }
  }

  call_trump(suit) {
    var call = mkmsg("call_trump", {"table_id": id, "suit": suit});
    tsocket.send(JSON.encode(call));
  }

  recv_trump_passed(seat) {
    recv_chat("Table", "Seat $seat passed on trump");
  }

  recv_trump_called(seat, suit) {
    askTrump = false;
    var suitStr = suit_toString(suit);
    recv_chat("Table", "Seat $seat called trump $suitStr");
  }

  recv_ask_card(seat) {
    recv_chat("Table", "Seat $seat asked to play a card");
  }

  play_card(value, suit) {
    var play = mkmsg("play_card", {"table_id": id, "card": {"value": value, "suit": suit}});
    tsocket.send(JSON.encode(play));
  }

  recv_play_card(seat_num, card) {
    var value = card['value'];
    var suit = card['suit'];
    var played_card = new Card(value, suit);

    if (seat_num == seat) {
      cards.remove(played_card);
    }
    recv_chat("Table", "Seat $seat_num played card $played_card");
  }

  recv_take_trick(seat_num) {
    recv_chat("Table", "Seat $seat_num took down the trick");
  }

  recv_game_cancel() {
    // TODO: move to a 'game' class
    cards.clear();
    askTrump = false;
    dealer = null;
  }

  call_run() {
    var call_run = mkmsg("call_run", {"table_id": id});
    tsocket.send(JSON.encode(call_run));
  }

  recv_call_run(seat_num, run_type) {
    recv_chat("Table", "Seat $seat_num called a run type $run_type");
  }

  show_run() {
    var show_run = mkmsg("show_run", {"table_id": id});
    tsocket.send(JSON.encode(show_run));
  }

  recv_show_run(seat_num, run_type, cards) {
    recv_chat("Table", "Seat $seat_num showed their run: $cards");
  }

  recv_noshow_run(seat_num, better_type, run_type, high_value, is_trump, other_seat) {
    recv_chat("Table", "Seat $seat_num was not able to show their run.");
    recv_chat("Table", "Reason $better_type at $other_seat");
  }

  play_bella() {
    var play_bella = mkmsg("play_bella", {"table_id": id});
    tsocket.send(JSON.encode(play_bella));
  }

  recv_call_bella(seat_num) {
    recv_chat("Table", "Seat $seat_num called bella.");
  }
}

class Card {
  int value;
  int suit;

  Card(this.value, this.suit);

  factory Card.from_json(json) {
    var value = json['value'];
    var suit = json['suit'];
    return new Card(value, suit);
  }

  String toString() {
    String valueStr;
    if (value <= 10) {
      valueStr = "$value";
    } else {
      var values = ["J", "Q", "K", "A"];
      valueStr = values[(value - 11)];
    }

    String suitStr = ["c", "d", "s", "h"][(suit - 1)];
    return "[$valueStr$suitStr]";
  }

  int get hashCode {
    int result = 17;
    result = 37 * result + value.hashCode;
    result = 37 * result + suit.hashCode;
    return result;
  }

  bool operator==(other) {
    if (identical(other, this)) return true;
    return (other.value == value && other.suit == suit);
  }

  ImageElement get imgElement {
    return new ImageElement(src: "../images/2.png");
    // return "../images/2.png";
  }
}


@CustomTag('tarabish-app')
class Tarabish extends PolymerElement {
  @observable bool isLoggedIn = false;
  @observable String newLoginName;
  @observable String loginName = "Nobody";

  @observable bool showTables = false;
  @observable List<TableView> tableViews = [];

  Tarabish.created() : super.created() {
    tsocket = new TarabishSocket("ws://127.0.0.1:42745/websocket");
    print ("Tarabish Created");
    tarabish = this;
  }

  // Lazy start socket on first login
  _setup_socket() {
    tsocket.init();
  }

  valid_login(name) {
    isLoggedIn = true;
    loginName = name;

    showTables = true;
    refreshTables();
  }

  update_lobby(tables) {
    tableViews = tables;
  }

  // Temporary  to test re-attach
  doDisconnect() {
    print("Disconnected");
    tsocket.webSocket.close();
  }

  doLogin() {
    _setup_socket();
    var login = mkmsg("login", {"name": newLoginName});
    tsocket.send(JSON.encode(login));
    print("Login called");
  }

  // TODO: add a @require_socket
  refreshTables() {
    tsocket.send(JSON.encode(mkmsg("get_tables")));
  }

  sit(Event event, var detail, var target) {
//    var table = int.parse(target.attributes['data-table']);
//    var seat = int.parse(target.attributes['data-seat']);
    var table = target.attributes['data-table'];
    var seat = target.attributes['data-seat'];


    var sit = mkmsg("sit", {
      "table_id": table,
      "seat": seat
    });
    tsocket.send(JSON.encode(sit));
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

suit_toString(suit) {
  return ["clubs", "diamonds", "spades", "hearts"][suit - 1];
}

///**
// * Learn about the Web UI package by visiting
// * http://www.dartlang.org/articles/dart-web-components/.
// */
void main() {
  print ("Main called");

  // Enable this to use Shadow DOM in the browser.
  //useShadowDom = true;

//  tarabish = new Tarabish();
//
////  CanvasElement _canvas = querySelectorAll("#tableCanvas");
////  var ttable = new TarabishCanvas(_canvas);
////  ttable.start();
//
////  querySelector('#disconnect').onClick.listen((e) => tarabish.do_disconnect(e));
}
