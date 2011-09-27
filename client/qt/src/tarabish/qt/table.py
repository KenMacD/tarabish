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

        self.north = CardWidget(resource_path, None, self)
        self.north.move(self.north_position)

        self.south = CardWidget(resource_path, None, self)
        self.south.move(self.south_position)

        self.east = CardWidget(resource_path, None, self)
        self.east.move(self.east_position)

        self.west = CardWidget(resource_path, None, self)
        self.west.move(self.west_position)

        self.trump_select = TrumpWidget(resource_path, self)
        self.trump_select.setAutoFillBackground(True)
        self.trump_select.selected.connect(self.trump_selected)

        rect = self.trump_select.frameGeometry()
        mid_point = self.geometry().center()
        rect.moveCenter(mid_point)
        self.trump_select.move(rect.topLeft())
        self.trump_select.hide()

    def _get_card_widget(self, position):
        if position == 0:
            return self.north
        elif position == 1:
            return self.east
        elif position == 2:
            return self.south
        else:
            return self.west

    def show_card(self, position, card):
        widget = self._get_card_widget(position)
        widget.set_card(card)

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

        self.img = QLabel(self)
        self.img.setFixedSize(CARD_WIDTH, CARD_HEIGHT)
        self.img.move(0,0)

        self.card_source = CardSource(resource_path)

        self.set_card(pyCard)

    def set_card(self, card):
        self.card = card
        if card:
            card_pixels = self.card_source.get_card(card.value, card.suit)
            self.img.setPixmap(card_pixels)
            self.img.show()
        else:
            self.img.hide()

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


class CardButtonWidget(QWidget):
    call_run = Signal()
    show_run = Signal()
    play_bella = Signal()

    def __init__(self, parent=None):
        super(CardButtonWidget, self).__init__(parent)

        self.call_run_button = QPushButton("Call Run")
        self.call_run_button.clicked.connect(self.call_run)

        self.show_run_button = QPushButton("Show Run")
        self.show_run_button.clicked.connect(self.show_run)

        self.bella_button    = QPushButton("Play Bella")
        self.bella_button.clicked.connect(self.play_bella)

        self.card_button_layout = QVBoxLayout()
        self.card_button_layout.addWidget(self.call_run_button)
        self.card_button_layout.addWidget(self.show_run_button)
        self.card_button_layout.addWidget(self.bella_button)

        self.setDisabled(True)
        self.setLayout(self.card_button_layout)

    def enable(self):
        self.setDisabled(False)

    def setDisabled(self, disable):
        super(CardButtonWidget, self).setDisabled(disable)

        self.call_run_button.setDisabled(disable)
        self.show_run_button.setDisabled(disable)
        self.bella_button.setDisabled(disable)


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

    def _seat_to_position(self, seat):
        return (2 - self.seat_num + seat) % 4

    def get_seat_display(self, seat):
        return self.seats[self._seat_to_position(seat)]

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

        self.card_buttons = CardButtonWidget()

        cards_and_button_box = QHBoxLayout()
        cards_and_button_box.addWidget(self.card_box)
        cards_and_button_box.addWidget(self.card_buttons)
        vbox.addLayout(cards_and_button_box)
        
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
        server.eventDispatcher.connect(EventType.CALL_TRUMP,
                self.handle_call_trump, table_id)
        server.eventDispatcher.connect(EventType.PLAY_CARD,
                self.handle_play_card, table_id)

        self.testsuit = 1
        self.testvalue = 6
        testButton.clicked.connect(self.testNewCard)
        testButton2.clicked.connect(self.testDelCard)

        self.card_buttons.call_run.connect(self.call_run)
        self.card_buttons.show_run.connect(self.show_run)
        self.card_buttons.play_bella.connect(self.play_bella)

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
        try:
            self.server.playCard(self.table_id, card)
            self.logger.append("Table %d you played card %s" % (self.table_id,
                str(card)))
        except InvalidOperation as exc:
            self.logger.append("Table %d could not play card %s because %s" %
                    (self.table_id, str(card), str(exc)))

    def select_trump(self, suit):
        try:
            self.server.callTrump(self.table_id, suit)
            self.table_top.hide_trump_select()
        except InvalidOperation as exc:
            self.logger.append("Table %d could not call trump: %s" %
                    (self.table_id, str(exc)))

    def call_run(self):
        try:
            self.server.callRun(self.table_id)
            self.logger.append("Table %d call run" % (self.table_id,))
        except InvalidOperation as exc:
            self.logger.append("Table %d call run failed: %s" % (self.table_id,
                str(exc)))

    def show_run(self):
        try:
            self.server.showRun(self.table_id)
            self.logger.append("Table %d show run" % (self.table_id,))
        except InvalidOperation as exc:
            self.logger.append("Table %d show run failed: %s" % (self.table_id,
                str(exc)))

    def play_bella(self):
        try:
            self.server.playBella(self.table_id)
            self.logger.append("Table %d played bella" % (self.table_id,))
        except InvalidOperation as exc:
            self.logger.append("Table %d play bella failed: %s" %
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

    def handle_call_trump(self, seat, suit):
        # TODO: translate number for suit to actual suit string
        self.logger.append("Table %d seat %d called %d" % (self.table_id,
            seat, suit))
        if suit:
            self._enable_play()

    def handle_play_card(self, seat, card):
        position = self._seat_to_position(seat)
        self.table_top.show_card(position, card)

    def _enable_play(self):
        self.card_buttons.enable()

    def _start_game(self):
        try:
            self.server.startGame(self.table_id)
            self.logger.append("Game started at table %d." % (self.table_id))
        except InvalidOperation, e:
            self.logger.append("Could not start game at table %d: %s" % (self.table_id, e.why))
