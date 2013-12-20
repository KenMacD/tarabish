import 'package:polymer/polymer.dart';
import 'dart:html';
import 'dart:async';

import 'package:tarabishdart/src/model.dart';
import 'package:tarabishdart/src/ui.dart';
import 'package:tarabishdart/src/tsocket.dart';

typedef void ChatFun(String name, String msg);



@CustomTag('the-table')
class TheTable extends CanvasElement with Polymer, Observable {

  CanvasRenderingContext2D _context;

  TarabishSocket _tsocket;

  Table model;
  int _seatNum;
  ChatFun _chatFunction; /* TODO: remove when chat is handled by the table */

  bool allLoaded = false;

  TrumpSelector trumpSelector;
  List<Clickable> clickable = [];
  List<Clickable> doubleClickable = [];

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
    this.onDoubleClick.listen(_onDoubleClick);

    var futures = initUI();

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


  void _redraw(num time) {
    this.clickable = [];
    this.doubleClickable = [];

    _context.fillStyle = "#27462c";
    _context.fillRect(0, 0, window.innerWidth, window.innerHeight);
    _context.fillStyle = "#000000";

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
      clickable.add(trumpSelector);
      trumpSelector.draw(_context);
    }

    if (model.game.southCard != null){
      var cardUI = new CardUI(476, 357, model.game.southCard);
      cardUI.draw(_context);
    }

    if (model.game.westCard != null) {
      var cardUI = new CardUI(394, 253, model.game.westCard);
      cardUI.draw(_context);
    }

    if (model.game.northCard != null) {
      var cardUI = new CardUI(476, 145, model.game.northCard);
      cardUI.draw(_context);
    }

    if (model.game.eastCard != null) {
      var cardUI = new CardUI(558, 253, model.game.eastCard);
      cardUI.draw(_context);
    }

    if (model.game.action != NONE) {
      var offset = model.game.action;
      var arrow;

      _context.save();
      switch (offset) {
        case WEST:
          arrow = 0x2190;
          break;
        case NORTH:
          arrow = 0x2191;
          break;
        case EAST:
          arrow = 0x2192;
          break;
        case SOUTH:
          arrow = 0x2193;
          break;
      }
      var oldFont = _context.font;
      _context.font = "40pt SansSerif";
      _context.textAlign = 'center';
      _context.fillText(new String.fromCharCode(arrow), 512, 311);
      _context.restore();
      _context.font = oldFont;
    }

    var x = 300;
    for (var card in model.game.cards) {
      var cardUI = new CardUI(x, 630, card);
      cardUI.draw(_context);
      doubleClickable.add(cardUI);
      x += 80;
    }
  }

  _update() {
    window.requestAnimationFrame(_redraw);
  }

  // TODO: handle z-height if anything ever overlaps.
  _onClick(MouseEvent event) {
    var x = event.offset.x;
    var y = event.offset.y;

    for (var element in clickable) {
      if (element.contains(x, y)) {
        element.clicked(x, y, _tsocket);
      }
    }
  }

  _onDoubleClick(MouseEvent event) {
    var x = event.offset.x;
    var y = event.offset.y;

    for (var element in doubleClickable) {
      if (element.contains(x, y)) {
        element.clicked(x, y, _tsocket);
      }
    }
  }
}