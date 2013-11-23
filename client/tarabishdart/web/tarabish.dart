import 'dart:html';
import 'dart:async';

import 'dart:convert';
import 'dart:math';

import 'package:polymer/polymer.dart';
import 'package:tarabishdart/src/model.dart';
import 'package:tarabishdart/src/tsocket.dart';

import 'the_table.dart';


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


bool DEBUG = true;



@CustomTag('tarabish-app')
class Tarabish extends PolymerElement {
  @observable bool isLoggedIn = false;
  @observable String newLoginName;
  @observable String loginName = "Nobody";

  @observable bool showTables = false;
  @observable List<TableView> tableViews = [];

  @observable bool showTable = true; /* TODO: should be false */
  TheTable _theTable;

  Tarabish.created() : super.created() {
    tsocket = new TarabishSocket("ws://127.0.0.1:42745/websocket",
        valid_login, update_lobby);
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
    var table = int.parse(target.attributes['data-table']);
    var seat = int.parse(target.attributes['data-seat']);

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
}
