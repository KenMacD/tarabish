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
            self.cards.append(item)

            self.cardLayout.insertWidget(self.cardLayout.count() - 1, item)

    def del_card(self, index):
        self.cards.pop(index)
        card = self.cardLayout.takeAt(index)
        card.widget().setParent(None)


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

        testButton = QPushButton("Create cards")
        testButton2 = QPushButton("Remove first card")
        vbox.addWidget(testButton)
        vbox.addWidget(testButton2)

        cards = [Card(10, CLUBS), Card(ACE, HEARTS)]
        self.cardBox = CardBoxWidget(cards)
        vbox.addWidget(self.cardBox)
        vbox.addLayout(hbox)

        self.setLayout(vbox)

        eventSignal.connect(self.handleEvent)

        testButton.clicked.connect(self.testNewCard)
        testButton2.clicked.connect(self.testDelCard)

    def handleEvent(self, event):
        self.logger.append("Table %d Received Event: %s" % (self.tableId,
            str(event)))

    def testNewCard(self):
        self.cardBox.add_cards([Card(JACK, SPADES), Card(9, DIAMONDS)])

    def testDelCard(self):
        self.cardBox.del_card(0)
