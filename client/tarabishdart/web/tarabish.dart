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

@CustomTag('tarabish-app')
class Tarabish extends PolymerElement with TarabishCallbacks {
  TarabishSocket tsocket;
  int _tableId;
  bool _seated = false;

  @observable bool isLoggedIn = false;
  @observable String newLoginName;
  @observable String loginName = "Nobody";

  @observable bool showTables = false;
  @observable List<TableView> tableViews = toObservable([]);

  // Used to display the chat
  @observable Table table = null;

  @observable String chatLine = "";

  @observable bool showTable = false; /* TODO: should be false */

  @observable TheTable theTable;
  //TheTable get table => _theTable;

  Tarabish.created() : super.created() {
    print ("Tarabish Created");
  }

  void attached() {
    super.attached();
    theTable = shadowRoot.querySelector("#thetable");
    tsocket = new TarabishSocket("ws://127.0.0.1:42745/websocket", this);
    print("Tarabish Entered View");
  }

  // Lazy start socket on first login
  _setup_socket() {
    tsocket.init();
  }

  validLogin(name) {
    isLoggedIn = true;
    loginName = name;

    showTables = true;
    refreshTables();
  }

  lobbyUpdate(tables) {
    tableViews.clear();
    tableViews.addAll(tables);
    //tableViews = tables;
  }

  // Temporary  to test re-attach
  doDisconnect() {
    print("Disconnected");
    tsocket.webSocket.close();
  }

  doLogin() {
    _setup_socket();
    tsocket.login(newLoginName);
    print("Login called");
  }

  // TODO: add a @require_socket
  refreshTables() {
    tsocket.getTables();
  }

  sit(Event event, var detail, var target) {
    _tableId = int.parse(target.attributes['data-table']);
    var seat = int.parse(target.attributes['data-seat']);

    tsocket.sit(_tableId, seat);
    print("Sit called $_tableId -- $seat");
  }


  youSat(Table table) {
    _seated = true;
    showTable = true;
    this.table = toObservable(table, deep: true);
    _tableId = table.id;
    //_theTable = shadowRoot.querySelector("#thetable");
    theTable.init(tsocket, table);
    return theTable;
  }

  sendChat() {
    if (_seated && chatLine.length > 0) {
      tsocket.sendChat(_tableId, chatLine);
      chatLine = "";
    }
  }

}