library models;

import 'dart:async';
import 'package:polymer/polymer.dart';

import 'package:tarabishdart/src/tsocket.dart';

const int PASS = 0;
const int CLUBS = 1;
const int DIAMONDS = 2;
const int SPADES = 3;
const int HEARTS = 4;

const int SOUTH  = 0;
const int WEST   = 1;
const int NORTH  = 2;
const int EAST   = 3;
const int NONE   = -1;

class SeatView extends Object with Observable{
  @observable bool isOpen;
  @observable String name;
  @observable int num;

  SeatView(this.isOpen, this.name, this.num);

  factory SeatView.from_json(json) {
    return new SeatView(json['isOpen'], json['name'], json['num']);
  }

  String toString() {
    if (isOpen) return "Seat open";
    else return "Seat occupied by $name";
  }

  sat(String name) {
    assert(isOpen == true);

   isOpen = false;
   this.name = name;
  }

  stood() {
    assert(isOpen == false);

    isOpen = true;
    this.name = "";
  }
}

//@observable
class TableView extends Object with Observable {
  @observable int tableId;
  @observable List<SeatView> seats;

  TableView(this.tableId, this.seats);

  factory TableView.from_json(json) {
    var seats = toObservable(new List<SeatView>());
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
}

// Note: sending methods are deprecated. Send directly to socket instead

class Game extends Object with Observable {
  int seat;
  List<Card> cards = toObservable(new List());

  int _dealer = NONE;
  int _action = NONE;

  bool askTrump = false;

  int get dealer => (_dealer == NONE)? NONE : _seatToLocation(_dealer);
  int get action => (_action == NONE)? NONE : _seatToLocation(_action);

  Card northCard = null;
  Card eastCard = null;
  Card southCard = null;
  Card westCard = null;

  Timer sweepTimer;
  int sweepDirection;

  List<List<int>> handScores = new List();
  List<int> score = new List();

  // TODO: remove this once observable works
  Table table;

  Game(this.seat, this.table);

  int _seatToLocation(int seatNum) {
    var offset = (seatNum - seat) % 4;
    return offset;
  }

  recvDeal(new_dealer, new_cards) {
    cards.addAll(new_cards);
    _dealer = new_dealer;

    print("Received new dealer $dealer and cards $new_cards");
  }

  recvAskTrump(seatNum) {
    table.seatChat("Table", seatNum, "asked to call trump");
    _action = seat;
    if (seat == this.seat) {
      askTrump = true;
    } else {
      askTrump = false;
    }
  }

  recvPlayCard(seatNum, card) {
    _action = NONE;
    var value = card['value'];
    var suit = card['suit'];
    var playedCard = new Card(value, suit);

    _sweep();

    if (seatNum == seat) {
      cards.remove(playedCard);
    }
    table.seatChat("Table", seatNum, "played card $playedCard");

    var offset = (seatNum - seat) % 4;
    switch (offset) {
      case SOUTH:
        southCard = new Card(value, suit);
        break;
      case WEST:
        westCard = new Card(value, suit);
        break;
      case NORTH:
        northCard = new Card(value, suit);
        break;
      case EAST:
        eastCard = new Card(value, suit);
        break;
    }
  }

  recvTrumpCalled(seatNum, suit) {
    askTrump = false;
    _action = NONE;
    var suitStr = suit_toString(suit);
    table.seatChat("Table", seatNum, "called trump $suitStr");
  }

  recvTakeTrick(seatNum) {
    _action = NONE;
    sweepDirection = _seatToLocation(seatNum);
    sweepTimer = new Timer(new Duration(seconds:2), () => _sweep());
    table.seatChat("Table", seatNum, "took down the trick");
  }

  recvAskCard(seatNum) {
    _action = seat;
    table.seatChat("Table", seatNum, "asked to play a card");
  }

  recvHandDone(handScores, score, bait) {
    _sweep();
    this.handScores.add(handScores);
    table.recvChat("Table", "Hand done $handScores");
  }

  _sweep() {
    if (sweepDirection == NONE) {
      return;
    }
    if (sweepTimer != null && sweepTimer.isActive) {
      sweepTimer.cancel();
      sweepTimer = null;
    }
    sweepDirection = NONE;
    northCard = eastCard = southCard = westCard = null;
  }
}

// TODO: why doesn't just @observable work here?
//@observable
class Table extends Object with Observable {
  int id;
  TableView view;
  int seat; // Your seat
  Game game;

  // TODO: make observable call work in the_table
  List<updatedCallback> updateCallbacks = new List();

  @observable
  String chatText = "";

  SeatView get west  => view.seats[(seat + 1) % 4];
  SeatView get north => view.seats[(seat + 2) % 4];
  SeatView get east  => view.seats[(seat + 3) % 4];

  Table(this.id, this.view, this.seat);

  String seatToName(int seatnum) {
    return view.seats[seatnum].name;
  }

  recvChat(name, message) {
    var text = "$name: $message";
    if (!chatText.isEmpty) {
      text = "${text}\n${chatText}";
    }
    chatText = text;
  }

  seatChat(name, seatNum, message) {
    var seatName = seatToName(seatNum);
    recvChat(name, "$seatName $message");
  }

  recvSit(seat_num, name) {
    var seat = view.seats.elementAt(seat_num);
    seat.sat(name);
    // TODO: refactor to recv_chat to accept server messages.
    recvChat("Table", "$name sat"); // TODO: print which seat
  }

//  part() {
//    var part = mkmsg("part_table", {"table_id": id});
//    tsocket.send(JSON.encode(part));
//  }

  recvPart(seat_num, name) {
    // TODO: clear game info
    assert(seat_num != this.seat);
    var seat = view.seats.elementAt(seat_num);
    seat.stood();
    recvChat("Table", "$name left the table");
  }


  recvTrumpPassed(seatNum) {
    seatChat("Table", seatNum, "passed on trump");
  }

  recv_new_game() {
    game = new Game(seat, this);
  }

  recv_game_cancel() {
    game = null;
  }

  recvCallRun(seatNum, runType) {
    switch (runType) {
      case 1:
        seatChat("Table", seatNum, "called a Twenty");
        break;
      case 2:
        seatChat("Table", seatNum, "called a Fifty");
        break;
    }
  }

  recvShowRun(seatNum, runType, cards) {
    seatChat("Table", seatNum, "showed their run: $cards");
  }

  recvNoshowRun(seatNum, betterType, runType, highValue, isTrump, otherSeat) {
    var otherName = seatToName(otherSeat);
    seatChat("Table", seatNum, "was not able to show their $highValue run, $otherName's is better.");
  }

  recv_call_bella(seatNum) {
    seatChat("Table", seatNum, "called bella.");
  }
}