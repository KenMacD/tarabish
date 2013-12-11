import 'package:polymer/polymer.dart';
import 'dart:html';
import 'dart:async';

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

  bool allLoaded = false;

  var cardImages = [];

  ImageElement suitsImage;

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

    this.onClick.listen(_onClick);

    var futures = [];

    // TODO: is there an easy way to wait on all these being loaded before allowing starting a game?
    for (var i = 1; i <= 36; i++) {
      var image = new ImageElement(src: "images/$i.png");
      cardImages.add(image);
      futures.add(image.onLoad.first);
    }

    suitsImage = new ImageElement(src: "images/suits.png");
    futures.add(suitsImage.onLoad.first);

    Future.wait(futures).then((_) {
      allLoaded = true;
      _update();
    });

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

    if (!allLoaded) {
      _context.fillText("Still Loading Images", 100, 110);
    }

    if (model == null) {
      return;
    }


    // Draw West
    _context.fillText(model.west.name, 10, 512);

    // Draw North
    _context.fillText(model.north.name, 500, 20);

    // Draw East
    _context.fillText(model.east.name, 900, 512);

    // Draw Middle
    if (model.askTrump) {
      _context.fillText("Please select Trump:", 412, 174);
      _context.drawImage(suitsImage, 412, 284);
      suitsImage.onClick.listen((event) => print("SUITS CLICKED!"));
    }

    var x = 300;
    for (var card in model.cards) {
      _context.drawImage(_getCardImage(card), x, 630);
      x += 80;
    }
  }

  void _update() {
    window.requestAnimationFrame(_redraw);
  }

  void _onClick(MouseEvent event) {
    var x = event.offset.x;
    var y = event.offset.y;

    if (model == null) {
      return;
    }

    if (model.askTrump && x >= 412 && x < 612 && y >= 284 && y < 484) {
      // S H
      // D C
      if (y < 384) {
        if (x < 512) _tsocket.callTrump(SPADES);
        else _tsocket.callTrump(HEARTS);
      } else {
        if (x < 512) _tsocket.callTrump(DIAMONDS);
        else _tsocket.callTrump(CLUBS);
      }
    }
  }
}