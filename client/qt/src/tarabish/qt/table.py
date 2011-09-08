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
        
    def _handle_chat_message(self, message):
        self.messages.append(message)

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
    def __init__(self, tableId, server, logger, parent=None):
        super(Table, self).__init__(parent)
        self.tableId = tableId
        self.logger = logger
        self.setWindowTitle("Tarabish Table %d"%(tableId))
        self.resize(800, 600)

        hbox = QHBoxLayout()
        hbox.addWidget(ChatWidget(server, self.tableId))

        vbox = QVBoxLayout()

        top_grid = QGridLayout()

        self.north_label = QLabel("<empty>")
        self.north_label.setAlignment(Qt.AlignCenter)
        top_grid.addWidget(self.north_label, 0, 1)

        self.west_label = QLabel("<empty>")
        self.west_label.setAlignment(Qt.AlignRight | Qt.AlignVCenter)
        top_grid.addWidget(self.west_label, 1, 0)

        self.east_label = QLabel("<empty>")
        self.east_label.setAlignment(Qt.AlignLeft | Qt.AlignVCenter)
        top_grid.addWidget(self.east_label, 1, 2)

        self.south_label = QLabel("<empty>")
        self.south_label.setAlignment(Qt.AlignCenter)
        top_grid.addWidget(self.south_label, 2, 1)

        table_top = TableTopWidget()
        top_grid.addWidget(table_top, 1, 1)

        vbox.addLayout(top_grid)

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
        self.logger.append("Table %d Playing card %s %s" % (self.tableId,
            str(card.value), str(card.suit)))

    def testNewCard(self):
        self.cardBox.add_cards([Card(JACK, SPADES), Card(9, DIAMONDS)])

    def testDelCard(self):
        self.cardBox.del_card(0)
        
    def handle_sit_event(self, name, table, seat):
        self.logger.append("TABLE: User %s sat at table %d in seat %d" % (name, table, seat))
