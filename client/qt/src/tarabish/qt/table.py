from functools import partial

from tarabish.thrift.constants import (CLUBS, SPADES, HEARTS, DIAMONDS)
from tarabish.thrift.constants import (JACK, QUEEN, KING, ACE)
from tarabish.thrift.ttypes import (Card, EventType, InvalidOperation)
from PySide.QtCore import (Signal, QSize, QPoint, Qt)
from PySide.QtGui import *

from cardlib import (get_card, CARD_WIDTH, CARD_HEIGHT)

class ChatWidget(QWidget):
    def __init__(self, server, table_id, parent=None):
        super(ChatWidget, self).__init__(parent)
        self.server = server
        self.table_id = table_id
        
        send_button = QPushButton("Send")
        self.message_box = QLineEdit()
        self.messages = QTextBrowser()

        chat_widgets = QHBoxLayout()
        chat_widgets.addWidget(self.message_box)
        chat_widgets.addWidget(send_button)

        chat_container = QVBoxLayout()
        chat_container.addWidget(self.messages)
        chat_container.addLayout(chat_widgets)
        self.setLayout(chat_container)

        send_button.clicked.connect(self._send_message)
        
        self.server.eventDispatcher.connect(EventType.CHAT,
                                            self._handle_chat_message,
                                            self.table_id)
        
    def _handle_chat_message(self, table, name, message):
        display = "%s: %s" % (name, message)
        self.messages.append(display)

    def _send_message(self):
        message = self.message_box.text()
        if not message:
            return

        self.server.chat(self.table_id, message)
        self.message_box.clear()

class TableTopWidget(QWidget):
    MARGIN = 6

    def __init__(self, resource_path, parent=None):
        super(TableTopWidget, self).__init__(parent)

        self.width = CARD_WIDTH * 3 + self.MARGIN * 2
        self.height = CARD_HEIGHT * 3 + self.MARGIN * 2
        self.setFixedSize(self.width, self.height)

        self.north_position = QPoint(CARD_WIDTH + self.MARGIN, 0)
        self.west_position = QPoint(0, CARD_HEIGHT + self.MARGIN)
        self.south_position = QPoint(CARD_WIDTH + self.MARGIN,
                CARD_HEIGHT * 2+ self.MARGIN * 2)
        self.east_position = QPoint(CARD_WIDTH  * 2 + self.MARGIN * 2,
                CARD_HEIGHT + self.MARGIN)

        n = CardWidget(resource_path, Card(6, SPADES), self)
        n.move(self.north_position)

        s = CardWidget(resource_path, Card(7, SPADES), self)
        s.move(self.south_position)

        e = CardWidget(resource_path, Card(8, SPADES), self)
        e.move(self.east_position)

        w = CardWidget(resource_path, Card(9, SPADES), self)
        w.move(self.west_position)

    def sizeHint(self):
        return self.minimumSizeHint()

    def minimumSizeHint(self):
        return QSize(self.width, self.height)


class CardWidget(QWidget):
    doubleclicked = Signal()

    suit = {CLUBS: "C", SPADES: "S", HEARTS: "H", DIAMONDS: "D"}
    value = {JACK: "J", QUEEN: "Q", KING: "K", ACE: "A"}

    def __init__(self, resource_path, pyCard, parent=None):
        super(CardWidget, self).__init__(parent)

        self.card = pyCard

        card_pixels = get_card(resource_path, pyCard.value, pyCard.suit)

        img = QLabel(self)
        img.setPixmap(card_pixels)
        img.setFixedSize(CARD_WIDTH, CARD_HEIGHT)
        img.move(0,0)
        img.show()

    def sizeHint(self):
        return self.minimumSizeHint()

    def minimumSizeHint(self):
        return QSize(CARD_WIDTH, CARD_HEIGHT)

    def mouseDoubleClickEvent(self, event):
        self.doubleclicked.emit()

class CardBoxWidget(QWidget):
    doubleclicked = Signal(Card)

    def __init__(self, resource_path, cards=None, trump=None, parent=None):
        super(CardBoxWidget, self).__init__(parent)

        self.resource_path = resource_path

        self.resize(200, 100)

        self.cardLayout = QHBoxLayout()
        self.setLayout(self.cardLayout)

        self.cards = []
        self.add_cards(cards)

        self.trump = trump

    # TODO: sort J, 9, A, 10 if trump.
    def _compare_card(self, card_item1, card_item2):
        card1 = card_item1.card
        card2 = card_item2.card
        if card1.suit != card2.suit:
            return card1.suit - card2.suit
        else:
            if card1.value == ACE:
                return 1
            elif card2.value == ACE:
                return -1
            elif card1.value == 10:
                return 1
            elif card2.value == 10:
                return -1
            else:
                return card1.value - card2.value

    def _reset_layout(self):
        while True:
            rm_card = self.cardLayout.takeAt(0)
            if not rm_card:
                break
        self.cardLayout.addStretch()

    def add_cards(self, cards):
        for card in cards:
            item = CardWidget(self.resource_path, card)
            item.doubleclicked.connect(partial(self.cardDoubleClickEvent, card))
            self.cards.append(item)

        # Remove all the cards:
        self._reset_layout()

        # Sort
        self.cards = sorted(self.cards, cmp=self._compare_card)

        # And add them back in:
        for card_item in self.cards:
            self.cardLayout.insertWidget(self.cardLayout.count() - 1, card_item)

    def cardDoubleClickEvent(self, card):
        self.doubleclicked.emit(card)

    def del_card(self, index):
        self.cards.pop(index)
        card = self.cardLayout.takeAt(index)
        card.widget().setParent(None)


class Table(QDialog):
    class SeatMapping(object):
        def __init__(self, align, x, y):
            self.x = x
            self.y = y
            self.align = align
            self.name = "<empty>"
            self.occupied = False

        def set_name(self, name):
            self.name = name

        def set_occupied(self, is_occupied):
            self.occupied = is_occupied

        def make_label(self):
            widget = QLabel(self.name)
            widget.setAlignment(self.align)
            return widget

    def __init__(self, table_id, seat_num, table_view, server, logger,
            resource_path, parent=None):
        super(Table, self).__init__(parent)
        self.table_id = table_id
        self.logger = logger
        self.server = server
        
        self.setWindowTitle("Tarabish Table %d"%(table_id))
        self.resize(800, 600)

        self.mapping = {}
        # North
        self.mapping[(seat_num + 2) % 4] = self.SeatMapping(
                Qt.AlignCenter, 0, 1)
        # South
        self.mapping[seat_num] = self.SeatMapping(Qt.AlignCenter, 2, 1)
        # East
        self.mapping[(seat_num - 1) % 4] = self.SeatMapping(
                Qt.AlignLeft | Qt.AlignVCenter, 1, 2)
        # West
        self.mapping[(seat_num + 1) % 4] = self.SeatMapping(
                Qt.AlignRight | Qt.AlignVCenter, 1, 0)

        for (num, seat) in enumerate(table_view.seats):
            self.mapping[num].set_occupied(not seat.isOpen)
            if not seat.isOpen:
                self.mapping[num].set_name(seat.name)

        self.start_game_button = QPushButton("Start Game")
        self.start_game_button.setEnabled(self.is_full())
        self.start_game_button.clicked.connect(self._start_game)

        vbox = QVBoxLayout()

        seat_grid = QGridLayout()

        for seat in self.mapping.values():
            seat_grid.addWidget(seat.make_label(), seat.x, seat.y)

        table_top = TableTopWidget(resource_path)
        seat_grid.addWidget(table_top, 1, 1)
        self.seat_grid = seat_grid

        vbox.addLayout(seat_grid)

        testButton = QPushButton("Create card")
        testButton2 = QPushButton("Remove first card")
        vbox.addWidget(testButton)
        vbox.addWidget(testButton2)

        self.card_box = CardBoxWidget(resource_path, [])
        self.card_box.doubleclicked.connect(self.play_card)
        vbox.addWidget(self.card_box)
        
        game_button_layout = QDialogButtonBox(Qt.Horizontal)
        game_button_layout.addButton(self.start_game_button, QDialogButtonBox.ActionRole)
        game_button_layout.setFixedWidth(120)
        game_button_layout.setCenterButtons(True)
        
        chat_and_buttons_box = QHBoxLayout()
        chat_and_buttons_box.addWidget(ChatWidget(server, self.table_id))
        chat_and_buttons_box.addWidget(game_button_layout)
        vbox.addLayout(chat_and_buttons_box)

        self.setLayout(vbox)

        server.eventDispatcher.connect(EventType.SIT, self.handle_sit_event)
        server.eventDispatcher.connect(EventType.STAND, self.handle_stand_event)
        server.eventDispatcher.connect(EventType.DEAL, self.handle_deal)

        self.testsuit = 1
        self.testvalue = 6
        testButton.clicked.connect(self.testNewCard)
        testButton2.clicked.connect(self.testDelCard)

    def is_full(self):
        for seat_map in self.mapping.itervalues():
            if not seat_map.occupied:
                return False
        return True

    def play_card(self, card):
        self.logger.append("Table %d Playing card %s %s" % (self.table_id,
            str(card.value), str(card.suit)))

    def testNewCard(self):
        self.card_box.add_cards([Card(self.testvalue, self.testsuit)])
        if self.testvalue == ACE and self.testsuit == 4:
            self.testvalue = 6
            self.testsuit = 1
        elif self.testvalue == ACE:
            self.testvalue = 6
            self.testsuit = self.testsuit + 1
        else:
            self.testvalue = self.testvalue + 1

    def testDelCard(self):
        self.card_box.del_card(0)
        
    def _change_seat_label(self, seat, name):
        mapping = self.mapping[seat]
        old_label = self.seat_grid.itemAtPosition(mapping.x, mapping.y).widget()
        old_label.hide()
        old_label.setParent(None)
        mapping.set_name(name)
        mapping.set_occupied(True)
        self.seat_grid.addWidget(mapping.make_label(), mapping.x, mapping.y)

    def handle_sit_event(self, name, table, seat):
        self.logger.append("TABLE: User %s sat at table %d in seat %d" % (name, table, seat))
        self._change_seat_label(seat, name)
        self.start_game_button.setEnabled(self.is_full())

    def handle_stand_event(self, name, table, seat):
        self.logger.append("TABLE: User %s stood from table %d seat %d" %(name,
            table, seat))
        self._change_seat_label(seat, "<empty>")

    def handle_deal(self, dealt): # Ignore seat (first deal seat), not used yet
        self.card_box.add_cards(dealt)

    def _start_game(self):
        try:
            self.server.startGame(self.table_id)
            self.logger.append("Game started at table %d." % (self.table_id))
        except InvalidOperation, e:
            self.logger.append("Could not start game at table %d: %s" % (self.table_id, e.why))
