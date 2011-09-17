from functools import partial

from tarabish.thrift.constants import (CLUBS, SPADES, HEARTS, DIAMONDS)
from tarabish.thrift.constants import (JACK, QUEEN, KING, ACE)
from tarabish.thrift.ttypes import (Card, EventType, InvalidOperation)

from PySide.QtCore import (Signal, QSize, QPoint, Qt)
from PySide.QtGui import *

from trumpwidget import TrumpWidget
from cardlib import (CardSource, CARD_WIDTH, CARD_HEIGHT)

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
    trump_selected = Signal(int)

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

        self.north = CardWidget(resource_path, Card(6, SPADES), self)
        self.north.move(self.north_position)

        self.south = CardWidget(resource_path, Card(7, SPADES), self)
        self.south.move(self.south_position)

        self.east = CardWidget(resource_path, Card(8, SPADES), self)
        self.east.move(self.east_position)

        self.west = CardWidget(resource_path, Card(9, SPADES), self)
        self.west.move(self.west_position)

        self.trump_select = TrumpWidget(resource_path, self)
        self.trump_select.setAutoFillBackground(True)
        self.trump_select.selected.connect(self.trump_selected)

        rect = self.trump_select.frameGeometry()
        mid_point = self.geometry().center()
        rect.moveCenter(mid_point)
        self.trump_select.move(rect.topLeft())
        self.trump_select.hide()

    def show_trump_select(self):
        self.north.hide()
        self.south.hide()
        self.east.hide()
        self.west.hide()
        self.trump_select.show()

    def hide_trump_select(self):
        self.north.show()
        self.south.show()
        self.east.show()
        self.west.show()
        self.trump_select.hide()

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

        card_source = CardSource(resource_path)
        card_pixels = card_source.get_card(pyCard.value, pyCard.suit)

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



class Table(QMainWindow):
    class SeatDisplay():
        def __init__(self, align, x, y):
            self.x = x
            self.y = y
            self.label = QLabel()
            self.label.setAlignment(align)
            self.stand()

        def update(self, seat_view):
            if seat_view.isOpen:
                self.stand()
            else:
                self.sit(seat_view.name)

        def stand(self):
            self.name = "<empty>"
            self.occupied = False
            self.label.setText(self.name)

        def sit(self, name):
            self.name = name
            self.occupied = True
            self.label.setText(self.name)

        def is_empty(self):
            return not self.occupied

        def get_label(self):
            return self.label

    def get_seat_display(self, seat):
        return self.seats[(2 - self.seat_num + seat) % 4]

    def __init__(self, table_id, seat_num, table_view, server, logger,
            resource_path, parent=None):
        super(Table, self).__init__(parent)

        self.setAttribute(Qt.WA_QuitOnClose, False)
        self.table_id = table_id
        self.logger = logger
        self.server = server
        self.seat_num = seat_num
        
        self.setWindowTitle("Tarabish Table %d"%(table_id))
        self.resize(800, 600)

        self.seats = []
        # North, East, South, West:
        self.seats.append(self.SeatDisplay(Qt.AlignCenter, 0, 1))
        self.seats.append(
                self.SeatDisplay(Qt.AlignLeft | Qt.AlignVCenter, 1, 2))
        self.seats.append(self.SeatDisplay(Qt.AlignCenter, 2, 1))
        self.seats.append(
                self.SeatDisplay(Qt.AlignRight | Qt.AlignVCenter, 1, 0))

        for (num, seat) in enumerate(table_view.seats):
            self.get_seat_display(num).update(seat)

        self.start_game_button = QPushButton("Start Game")
        self.start_game_button.setEnabled(self.is_full())
        self.start_game_button.clicked.connect(self._start_game)

        vbox = QVBoxLayout()

        seat_grid = QGridLayout()
        for seat in self.seats:
            seat_grid.addWidget(seat.get_label(), seat.x, seat.y)

        self.table_top = TableTopWidget(resource_path)
        self.table_top.trump_selected.connect(self.select_trump)

        seat_grid.addWidget(self.table_top, 1, 1)
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

        main = QFrame()
        main.setLayout(vbox)
        self.setCentralWidget(main)

        server.eventDispatcher.connect(EventType.SIT, self.handle_sit_event, table_id)
        server.eventDispatcher.connect(EventType.STAND, self.handle_stand_event, table_id)
        server.eventDispatcher.connect(EventType.DEAL, self.handle_deal,
                table_id)
        server.eventDispatcher.connect(EventType.ASK_TRUMP,
                self.handle_ask_trump, table_id)

        self.testsuit = 1
        self.testvalue = 6
        testButton.clicked.connect(self.testNewCard)
        testButton2.clicked.connect(self.testDelCard)

    def closeEvent(self, event):
        # TODO For now we assume that if you are looking at a table,
        # then you are sitting. When observers are allowed
        message_box = self._create_warning_box()
        answer = message_box.exec_()
        
        if answer == QMessageBox.Ok:
            self.server.partTable(self.table_id)
            event.accept()
        else:
            event.ignore()

    def _create_warning_box(self):
        message_box = QMessageBox(self)
        message_box.setText("You are about to leave this table, and quit the game.")
        message_box.setInformativeText("Are you sure you would like to leave the game?")
        message_box.setStandardButtons(QMessageBox.Ok | QMessageBox.Cancel)
        message_box.setDefaultButton(QMessageBox.Cancel)
        message_box.setIcon(QMessageBox.Warning)
        return message_box

    def is_full(self):
        for seat in self.seats:
            if seat.is_empty():
                return False
        return True

    def play_card(self, card):
        self.logger.append("Table %d Playing card %s %s" % (self.table_id,
            str(card.value), str(card.suit)))

    def select_trump(self, suit):
        try:
            self.server.callTrump(self.table_id, suit)
            self.table_top.hide_trump_select()
        except InvalidOperation as exc:
            self.logger.append("Table %d could not call trump: %s" %
                    (self.table_id, str(exc)))

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

    def handle_sit_event(self, name, table, seat):
        self.logger.append("TABLE: User %s sat at table %d in seat %d" % (name, table, seat))
        self.get_seat_display(seat).sit(name)
        self.start_game_button.setEnabled(self.is_full())

    def handle_stand_event(self, name, table, seat):
        self.logger.append("TABLE: User %s stood from table %d seat %d" %(name,
            table, seat))
        self.get_seat_display(seat).stand()
        self.start_game_button.setEnabled(self.is_full())

    def handle_deal(self, dealt): # Ignore seat (first deal seat), not used yet
        self.card_box.add_cards(dealt)

    def handle_ask_trump(self, seat):
        if seat == self.seat_num:
            self.table_top.show_trump_select()
        else:
            self.logger.append("TABLE: Seat %d asked trump" % (seat))

    def _start_game(self):
        try:
            self.server.startGame(self.table_id)
            self.logger.append("Game started at table %d." % (self.table_id))
        except InvalidOperation, e:
            self.logger.append("Could not start game at table %d: %s" % (self.table_id, e.why))
