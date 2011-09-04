from tarabish.thrift.constants import (CLUBS, SPADES, HEARTS, DIAMONDS)
from tarabish.thrift.constants import (JACK, QUEEN, KING, ACE)
from tarabish.thrift.ttypes import (Card)
from PySide.QtCore import QSize
from PySide.QtGui import *

class ChatWidget(QWidget):
    def __init__(self):
        super(ChatWidget, self).__init__()
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

    def _send_message(self):
        # TODO [mstead] Send message via server
        self.messages.append(self.message_box.text())
        self.message_box.clear()

class CardWidget(QWidget):
    suit = {CLUBS: "C", SPADES: "S", HEARTS: "H", DIAMONDS: "D"}
    value = {JACK: "J", QUEEN: "Q", KING: "K", ACE: "A"}

    def __init__(self, pyCard, parent=None):
        super(CardWidget, self).__init__(parent)

        self.card = pyCard
        frame = QFrame(self)
        frame.setFrameStyle(QFrame.Box);
        frame.setFixedSize(70, 90)

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
        return QSize(70, 90)


class CardBoxWidget(QWidget):
    def __init__(self, cards=None, trump=None, parent=None):
        super(CardBoxWidget, self).__init__(parent)

        self.cardLayout = QHBoxLayout()
        self.resize(200, 100)

        if not cards:
            self.cards = []
        else:
            self.cards = cards
            self.add_cards(cards)

        self.trump = trump
        self.cardLayout.addStretch()

        self.setLayout(self.cardLayout)

    def add_cards(self, cards):
        for card in cards:
            item = CardWidget(card)
            self.cardLayout.addWidget(item)

        self.cards = self.cards.append(cards)


class Table(QDialog):
    def __init__(self, tableId, eventSignal, logger, parent=None):
        super(Table, self).__init__(parent)

        self.tableId = tableId
        self.logger = logger

        self.setWindowTitle("Tarabish Table %d"%(tableId))
        self.resize(800, 600)

        hbox = QHBoxLayout()
        hbox.addWidget(ChatWidget())

        vbox = QVBoxLayout()

        cards = [Card(10, CLUBS), Card(ACE, HEARTS)]
        vbox.addWidget(CardBoxWidget(cards))
        vbox.addLayout(hbox)

        self.setLayout(vbox)

        eventSignal.connect(self.handleEvent)

    def handleEvent(self, event):
        self.logger.append("Table %d Received Event: %s" % (self.tableId,
            str(event)))
