from functools import partial

from tarabish.thrift.constants import (CLUBS, SPADES, HEARTS, DIAMONDS)
from tarabish.thrift.constants import (JACK, QUEEN, KING, ACE)
from tarabish.thrift.ttypes import (Card, EventType)
from PySide.QtCore import (Signal, QSize, QPoint, Qt)
from PySide.QtGui import *

# General cards seem to be 2.5" by 3.5", so match that ratio
CARD_WIDTH = 64
CARD_HEIGHT = 90

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
        
        self.server.eventDispatcher.connect(EventType.CHAT, self._handle_chat_message)
        
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

    def __init__(self, parent=None):
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

        n = CardWidget(Card(2, SPADES), self)
        n.move(self.north_position)

        s = CardWidget(Card(4, SPADES), self)
        s.move(self.south_position)

        e = CardWidget(Card(3, SPADES), self)
        e.move(self.east_position)

        w = CardWidget(Card(5, SPADES), self)
        w.move(self.west_position)

    def sizeHint(self):
        return self.minimumSizeHint()

    def minimumSizeHint(self):
        return QSize(self.width, self.height)


class CardWidget(QWidget):
    doubleclicked = Signal()

    suit = {CLUBS: "C", SPADES: "S", HEARTS: "H", DIAMONDS: "D"}
    value = {JACK: "J", QUEEN: "Q", KING: "K", ACE: "A"}

    def __init__(self, pyCard, parent=None):
        super(CardWidget, self).__init__(parent)

        self.card = pyCard
        frame = QFrame(self)
        frame.setFrameStyle(QFrame.Box);
        frame.setFixedSize(CARD_WIDTH, CARD_HEIGHT)

        suit = CardWidget.suit[pyCard.suit]
        if pyCard.value > 10:
            value = CardWidget.value[pyCard.value]
        else:
            value = str(pyCard.value)

        value = "%s %s" %(value, suit)

        valueLabel = QLabel(value, self)
        valueLabel.move(1, 0)

    def sizeHint(self):
        return self.minimumSizeHint()

    def minimumSizeHint(self):
        return QSize(CARD_WIDTH, CARD_HEIGHT)

    def mouseDoubleClickEvent(self, event):
        self.doubleclicked.emit()

class CardBoxWidget(QWidget):
    doubleclicked = Signal(Card)

    def __init__(self, cards=None, trump=None, parent=None):
        super(CardBoxWidget, self).__init__(parent)

        self.resize(200, 100)

        self.cardLayout = QHBoxLayout()
        self.cardLayout.addStretch()
        self.setLayout(self.cardLayout)

        self.cards = []
        self.add_cards(cards)

        self.trump = trump

    def add_cards(self, cards):
        for card in cards:
            item = CardWidget(card)
            item.doubleclicked.connect(partial(self.cardDoubleClickEvent, card))
            self.cards.append(item)

            self.cardLayout.insertWidget(self.cardLayout.count() - 1, item)

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

        def set_name(self, name):
            self.name = name

        def make_label(self):
            widget = QLabel(self.name)
            widget.setAlignment(self.align)
            return widget

    def __init__(self, table_id, seat_num, table_view, server, logger, parent=None):
        super(Table, self).__init__(parent)
        self.table_id = table_id
        self.logger = logger
        self.setWindowTitle("Tarabish Table %d"%(table_id))
        self.resize(800, 600)

        hbox = QHBoxLayout()
        hbox.addWidget(ChatWidget(server, self.table_id))

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
            if not seat.isOpen:
                self.mapping[num].set_name(seat.name)

        vbox = QVBoxLayout()

        seat_grid = QGridLayout()

        for seat in self.mapping.values():
            seat_grid.addWidget(seat.make_label(), seat.x, seat.y)

        table_top = TableTopWidget()
        seat_grid.addWidget(table_top, 1, 1)
        self.seat_grid = seat_grid

        vbox.addLayout(seat_grid)

        testButton = QPushButton("Create cards")
        testButton2 = QPushButton("Remove first card")
        vbox.addWidget(testButton)
        vbox.addWidget(testButton2)

        cards = [Card(10, CLUBS), Card(ACE, HEARTS)]
        self.cardBox = CardBoxWidget(cards)
        self.cardBox.doubleclicked.connect(self.play_card)
        vbox.addWidget(self.cardBox)
        vbox.addLayout(hbox)

        self.setLayout(vbox)

        server.eventDispatcher.connect(EventType.SIT, self.handle_sit_event)

        testButton.clicked.connect(self.testNewCard)
        testButton2.clicked.connect(self.testDelCard)

    def play_card(self, card):
        self.logger.append("Table %d Playing card %s %s" % (self.table_id,
            str(card.value), str(card.suit)))

    def testNewCard(self):
        self.cardBox.add_cards([Card(JACK, SPADES), Card(9, DIAMONDS)])

    def testDelCard(self):
        self.cardBox.del_card(0)
        
    def handle_sit_event(self, name, table, seat):
        mapping = self.mapping[seat]
        old_label = self.seat_grid.itemAtPosition(mapping.x, mapping.y).widget()
        old_label.hide()
        old_label.setParent(None)
        mapping.set_name(name)
        self.seat_grid.addWidget(mapping.make_label(), mapping.x, mapping.y)

        self.logger.append("TABLE: User %s sat at table %d in seat %d" % (name, table, seat))
