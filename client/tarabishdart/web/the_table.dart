import 'package:polymer/polymer.dart';
import 'dart:html';

import 'package:tarabishdart/src/model.dart';
import 'package:tarabishdart/src/tsocket.dart';

typedef void ChatFun(String name, String msg);

@CustomTag('the-table')
class TheTable extends CanvasElement with Polymer, Observable, TableCallbacks {

  CanvasRenderingContext2D _context;

  TarabishSocket _tsocket;
  int _tableId;
  TableView _tableModel;
  int _seatNum;
  ChatFun _chatFunction; /* TODO: remove when chat is handled by the table */

  TheTable.created() : super.created() {
    print("TheTable Created");
    /* TODO: fill in */
  }

  void enteredView() {
    super.enteredView();
    print("TheTable Entered View");
    _context = getContext("2d");
    _update();
  }

  void init(TarabishSocket tsocket, int tableId, TableView tableView, int seatNum,
            ChatFun chatFunction) {
    // TODO: fill in more
    _tsocket = tsocket;
    _tableId = tableId;
    _tableModel = tableView;
    _seatNum = seatNum;
    _chatFunction = chatFunction;
    _update();
  }

  void _redraw(num time) {
    _context.fillStyle = "#27462c";
    _context.fillRect(0, 0, window.innerWidth, window.innerHeight);

    _context.fillStyle = "#000000";
    _context.fillText("TESTING", 100, 100);

  }

  void _update() {
    window.requestAnimationFrame(_redraw);
  }

  void recvSit(int seat, String name) {
    print("Received Sit");
    // TODO: fill in
    _chatFunction("Table", "$name sat at seat $seat");
  }

  void recvPart(int seat, String name) {
    // TODO: fill in
    _chatFunction("Table", "$name left seat $seat");
  }
}