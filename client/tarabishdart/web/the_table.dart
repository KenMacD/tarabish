import 'package:polymer/polymer.dart';
import 'dart:html';
import 'dart:async';

import 'package:tarabishdart/src/model.dart';
import 'package:tarabishdart/src/tsocket.dart';

typedef void ChatFun(String name, String msg);

abstract class Clickable {
  int get x;
  int get y;
  int get width;
  int get height;

  bool contains(int x, int y) {
    if (x < this.x || x > this.x + width)
      return false;
    if (y < this.y || y > this.y + height)
      return false;
    return true;
  }

  void clicked(int x, int y, TarabishSocket);
}

abstract class Drawable {
  void draw(CanvasRenderingContext2D _context);
}

class TrumpSelector extends Object with Drawable, Clickable {
  int x = 412;
  int y = 284;
  int width = 200;
  int height = 260;

  ImageElement suitsImage;

  TrumpSelector() {
    suitsImage = new ImageElement(src: "images/suits.png");

  }

  void draw(CanvasRenderingContext2D context) {
    //context.fillText("Please select Trump:", 412, 174);
    context.drawImage(suitsImage, 412, 284);
    var oldFont = context.font;
    context.font = "normal 60px Sans-Serif";
    context.textAlign = 'center';
    context.fillText("PASS", 512, 534);
    context.font = oldFont;
  }

  void clicked(int x, int y, TarabishSocket socket) {
    assert(this.contains(x, y));

    if (x >= this.x && x < this.x + this.width
        && y >= this.y && y < (484 + 60)) {
      // S H
      // D C
      if (y < 384) {
        if (x < 512) socket.callTrump(SPADES);
        else socket.callTrump(HEARTS);
      } else if (y < 484) {
        if (x < 512) socket.callTrump(DIAMONDS);
        else socket.callTrump(CLUBS);
      } else {
        socket.callTrump(PASS);
      }
    }
  }
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

  TrumpSelector trumpSelector;


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

    trumpSelector = new TrumpSelector();
    futures.add(trumpSelector.suitsImage.onLoad.first);
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

    if (model.game == null) {
      return;
    }

    // Draw Middle
    if (model.game.askTrump) {
      trumpSelector.draw(_context);
    }

    var x = 300;
    for (var card in model.game.cards) {
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

    if (model.game == null) {
      return;
    }

    if (model.game.askTrump && trumpSelector.contains(x, y)) {
      trumpSelector.clicked(x, y, _tsocket);
    }

  }
}