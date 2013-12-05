import 'package:polymer/polymer.dart';
import 'dart:html';

import 'package:tarabishdart/src/model.dart';
import 'package:tarabishdart/src/tsocket.dart';

typedef void ChatFun(String name, String msg);

class Seat {

}

@CustomTag('the-table')
class TheTable extends CanvasElement with Polymer, Observable {

  CanvasRenderingContext2D _context;

  TarabishSocket _tsocket;

  Table model;
  int _seatNum;
  ChatFun _chatFunction; /* TODO: remove when chat is handled by the table */

  var cardImages = [];

  // All in the tableModel, but just to make it easier.
  SeatView _west;
  SeatView _north;
  SeatView _east;

  TheTable.created() : super.created() {
    print("TheTable Created");
    /* TODO: fill in */
  }

  void enteredView() {
    super.enteredView();
    print("TheTable Entered View");
    _context = getContext("2d");

    // TODO: is there an easy way to wait on all these being loaded before allowing starting a game?
    for (var i = 1; i <= 36; i++) {
      cardImages.add(new ImageElement(src: "images/$i.png"));
    }

    _update();
  }

  void init(TarabishSocket tsocket, Table table) {
    // TODO: fill in more
    _tsocket = tsocket;
    model = table;

    model.registerUpdateCallback(this._update);
    _update();
  }

  ImageElement _getCardImage(Card card) {
    var num = (14 - card.value) * 4;
    // Order of suits is different from our suites
    // C - S - H - D
    switch (card.suit) {
      case CLUBS:
        num += 0;
        break;
      case SPADES:
        num += 1;
        break;
      case HEARTS:
        num += 2;
        break;
      case DIAMONDS:
        num += 3;
        break;
    }
    return cardImages[num];
  }

  void _redraw(num time) {
    _context.fillStyle = "#27462c";
    _context.fillRect(0, 0, window.innerWidth, window.innerHeight);

    _context.fillStyle = "#000000";
    _context.fillText("TESTING", 100, 100);

    if (model == null) {
      return;
    }

    // Draw West
    _context.fillText(model.west.name, 10, 512);

    // Draw North
    _context.fillText(model.north.name, 500, 20);

    // Draw East
    _context.fillText(model.east.name, 900, 512);

    var x = 300;
    for (var card in model.cards) {
      _context.drawImage(_getCardImage(card), x, 630);
      x += 80;
    }
  }

  void _update() {
    window.requestAnimationFrame(_redraw);
  }


}