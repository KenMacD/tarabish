library models;

import 'package:polymer/polymer.dart';

import 'package:tarabishdart/src/tsocket.dart';

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

// Note: sending methods are deprecated. Send directly to socket instead

// TODO: why doesn't just @observable work here?
//@observable
class Table extends Object with Observable {
  int id;
  TableView view;
  int seat; // Your seat

  List<Card> cards = toObservable(new List());
  int dealer;

  bool askTrump = false;

  // TODO: make observable call work in the_table
  List<updatedCallback> updateCallbacks = new List();

  @observable
  String chatText = "";

  SeatView get west  => view.seats[(seat + 1) % 4];
  SeatView get north => view.seats[(seat + 2) % 4];
  SeatView get east  => view.seats[(seat + 3) % 4];

  Table(this.id, this.view, this.seat);

  void registerUpdateCallback(updatedCallback method) {
    updateCallbacks.add(method);
  }


  void _changed() {
    for (var callback in updateCallbacks) {
      callback();
    }
  }

  recvChat(name, message) {
    var text = "$name: $message";
    if (!chatText.isEmpty) {
      text = "${text}\n${chatText}";
    }
    chatText = text;
  }

  recvSit(seat_num, name) {
    var seat = view.seats.elementAt(seat_num);
    seat.sat(name);
    // TODO: refactor to recv_chat to accept server messages.
    recvChat("Table", "$name sat"); // TODO: print which seat
    _changed();
  }

  part() {
    var part = mkmsg("part_table", {"table_id": id});
    tsocket.send(JSON.encode(part));
  }

  recvPart(seat_num, name) {
    assert(seat_num != this.seat);
    var seat = view.seats.elementAt(seat_num);
    seat.stood();
    recvChat("Table", "$name left the table");
    _changed();
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
    recvChat("Table", "Seat $seat asked to call trump");
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
    recvChat("Table", "Seat $seat passed on trump");
  }

  recv_trump_called(seat, suit) {
    askTrump = false;
    var suitStr = suit_toString(suit);
    recvChat("Table", "Seat $seat called trump $suitStr");
  }

  recv_ask_card(seat) {
    recvChat("Table", "Seat $seat asked to play a card");
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
    recvChat("Table", "Seat $seat_num played card $played_card");
  }

  recv_take_trick(seat_num) {
    recvChat("Table", "Seat $seat_num took down the trick");
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
    recvChat("Table", "Seat $seat_num called a run type $run_type");
  }

  show_run() {
    var show_run = mkmsg("show_run", {"table_id": id});
    tsocket.send(JSON.encode(show_run));
  }

  recv_show_run(seat_num, run_type, cards) {
    recvChat("Table", "Seat $seat_num showed their run: $cards");
  }

  recv_noshow_run(seat_num, better_type, run_type, high_value, is_trump, other_seat) {
    recvChat("Table", "Seat $seat_num was not able to show their run.");
    recvChat("Table", "Reason $better_type at $other_seat");
  }

  play_bella() {
    var play_bella = mkmsg("play_bella", {"table_id": id});
    tsocket.send(JSON.encode(play_bella));
  }

  recv_call_bella(seat_num) {
    recvChat("Table", "Seat $seat_num called bella.");
  }
}