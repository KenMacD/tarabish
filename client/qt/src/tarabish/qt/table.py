from functools import partial

from tarabish.thrift.constants import (CLUBS, SPADES, HEARTS, DIAMONDS)
from tarabish.thrift.constants import (JACK, QUEEN, KING, ACE)
from tarabish.thrift.ttypes import (Card, EventType, InvalidOperation)

from PySide.QtCore import (Signal, QSize, QPoint, Qt, QPropertyAnimation,
        QParallelAnimationGroup)
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
        
    def append(self, message):
        display = "Server: %s" % (message)
        self.messages.append(display)

    def _handle_chat_message(self, table, name, message):
        display = "%s: %s" % (name, message)
        self.messages.append(display)

    def _send_message(self):
        message = self.message_box.text()
        if not message:
            return

        self.server.chat(self.table_id, message)
        self.message_box.clear()

class TableTopCard(object):
    def __init__(self, resource_path, position, parent):
        self.position = position
        self.parent = parent
        self.resource_path = resource_path
        self.card_widget = CardWidget(self.resource_path, None, self.parent)
        self.card_widget.move(self.position)
        self.card_widget.show()

    def pop(self):
        widget = self.card_widget
        self.card_widget = CardWidget(self.resource_path, None, self.parent)
        self.card_widget.move(self.position)
        self.card_widget.show()
        return widget

    def set_card(self, card):
        self.card_widget.set_card(card)

    def hide(self):
        self.card_widget.hide()

    def show(self):
        self.card_widget.show()

    def raise_(self):
        self.card_widget.raise_()

class TableTopWidget(QWidget):
    MARGIN = 6
    trump_selected = Signal(int)

    def __init__(self, resource_path, parent=None):
        super(TableTopWidget, self).__init__(parent)

        self.width = CARD_WIDTH * 3 + self.MARGIN * 2
        self.height = CARD_HEIGHT * 3 + self.MARGIN * 2
        self.setFixedSize(self.width, self.height)

        self.cards = []
        self.cards.append(TableTopCard(resource_path, 
                QPoint(CARD_WIDTH + self.MARGIN, 0), self))
        self.cards.append(TableTopCard(resource_path, 
                QPoint(CARD_WIDTH  * 2 + self.MARGIN * 2,
                CARD_HEIGHT + self.MARGIN), self))
        self.cards.append(TableTopCard(resource_path,
                QPoint(CARD_WIDTH + self.MARGIN,
                CARD_HEIGHT * 2+ self.MARGIN * 2), self))
        self.cards.append(TableTopCard(resource_path,
                QPoint(0, CARD_HEIGHT + self.MARGIN), self))

        self.trump_select = TrumpWidget(resource_path, self)
        self.trump_select.setAutoFillBackground(True)
        self.trump_select.selected.connect(self.trump_selected)

        rect = self.trump_select.frameGeometry()
        mid_point = self.geometry().center()
        rect.moveCenter(mid_point)
        self.trump_select.move(rect.topLeft())
        self.trump_select.hide()

    def clear_sweep(self, pos):
        group = QParallelAnimationGroup()
        end_pos = self.cards[pos].card_widget.geometry()
        for card in self.cards:
            widget = card.pop()
            widget.raise_()
            ani = QPropertyAnimation(widget, "geometry")
            ani.setDuration(750)
            ani.setEndValue(end_pos)
            ani.finished.connect(partial(widget.setParent, None))
            group.addAnimation(ani)
        group.start()
        self.ani_group = group # Save to keep from being cleaned up

    def clear(self):
        self.hide_trump_select()
        for card in self.cards:
            card.set_card(None)

    def show_card(self, position, card):
        self.cards[position].set_card(card)

    def show_trump_select(self):
        for card in self.cards:
            card.hide()
        self.trump_select.show()

    def hide_trump_select(self):
        self.trump_select.hide()
        for card in self.cards:
            card.show()

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
        self.cardLayout.addStretch()

        self.cards = {}
        self.add_cards(cards)

        self.trump = trump

    # TODO: sort J, 9, A, 10 if trump.
    def _compare_card(self, card_widget1, card_widget2):
        card1 = card_widget1.card
        card2 = card_widget2.card
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

    def clear(self):
        self._reset_layout()
        for card_widget in self.cards.values():
            card_widget.setParent(None)
        self.cards = {}

    def _reset_layout(self):
        for card_widget in self.cards.values():
            self.cardLayout.removeWidget(card_widget)

    def add_cards(self, cards):
        # Remove all the cards:
        self._reset_layout()

        for card in cards:
            item = CardWidget(self.resource_path, card)
            item.doubleclicked.connect(partial(self.cardDoubleClickEvent, card))
            self.cards[card] = item

        # Sort
        sorted_card_widgets = sorted(self.cards.values(), cmp=self._compare_card)

        # And add them back in:
        for card_widget in sorted_card_widgets:
            self.cardLayout.insertWidget(self.cardLayout.count() - 1,
                    card_widget)

    def cardDoubleClickEvent(self, card):
        self.doubleclicked.emit(card)

    def rm_card(self, card):
        # Cards have different __hash__
        for (our_card, card_widget) in self.cards.items():
            if our_card == card:
                card_widget.setParent(None)
                del self.cards[our_card]
                return
        raise KeyError

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

    def _seat_to_name(self, seat):
        names = ['north', 'east', 'south', 'west']
        return names[self._seat_to_position(seat)]

    def get_seat_display(self, seat):
        return self.seats[self._seat_to_position(seat)]

    # TODO: logger no longer needed?
    def __init__(self, table_id, seat_num, table_view, server, logger,
            resource_path, parent=None):
        super(Table, self).__init__(parent)

        self.setAttribute(Qt.WA_QuitOnClose, False)
        self.table_id = table_id
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
        
        self.chat_widget = ChatWidget(server, self.table_id)
        self.logger = self.chat_widget

        chat_and_buttons_box = QHBoxLayout()
        chat_and_buttons_box.addWidget(self.chat_widget)
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
        server.eventDispatcher.connect(EventType.GAME_DONE,
                self.handle_game_done, table_id)
        server.eventDispatcher.connect(EventType.GAME_CANCEL,
                self.handle_game_cancel, table_id)
        server.eventDispatcher.connect(EventType.TAKE_TRICK,
                self.handle_take_trick, table_id)

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
        except InvalidOperation as exc:
            self.logger.append("Can not play card %s because %s" %
                    (str(card), str(exc)))

    def select_trump(self, suit):
        try:
            self.server.callTrump(self.table_id, suit)
            self.table_top.hide_trump_select()
        except InvalidOperation as exc:
            self.logger.append("Can not call trump because: %s" % (str(exc),))

    def call_run(self):
        try:
            self.server.callRun(self.table_id)
        except InvalidOperation as exc:
            self.logger.append("Can not call run because: %s" % (str(exc),))

    def show_run(self):
        try:
            self.server.showRun(self.table_id)
        except InvalidOperation as exc:
            self.logger.append("Can not show run because: %s" % (str(exc),))

    def play_bella(self):
        try:
            self.server.playBella(self.table_id)
        except InvalidOperation as exc:
            self.logger.append("Can not play bella because: %s" % (str(exc),))

    def handle_sit_event(self, name, table, seat):
        self.logger.append("User %s sat" % (name,))
        self.get_seat_display(seat).sit(name)
        self.start_game_button.setEnabled(self.is_full())

    def handle_stand_event(self, name, table, seat):
        self.logger.append("User %s stood" %(name))
        self.get_seat_display(seat).stand()
        self.start_game_button.setEnabled(self.is_full())

    def handle_deal(self, dealt): # Ignore seat (first deal seat), not used yet
        self.card_box.add_cards(dealt)

    def handle_ask_trump(self, seat):
        if seat == self.seat_num:
            self.table_top.show_trump_select()
        else:
            self.logger.append("%s asked to call trump" %
                    (self._seat_to_name(seat),))

    def handle_call_trump(self, seat, suit):
        # TODO: translate number for suit to actual suit string
        self.logger.append("%s called trump as <b>%d</b>" %
                (self._seat_to_name(seat), suit))
        if suit:
            self._enable_play()

    def handle_play_card(self, seat, card):
        if seat == self.seat_num:
            self.card_box.rm_card(card)

        position = self._seat_to_position(seat)
        self.table_top.show_card(position, card)

    def handle_game_done(self):
        self.handle_game_cancel()

    def handle_game_cancel(self):
        self.card_box.clear()
        self.table_top.clear()

    def handle_take_trick(self, seat):
        self.logger.append("%s takes trick" % (self._seat_to_name(seat),))
        position = self._seat_to_position(seat)
        self.table_top.clear_sweep(position)

    def _enable_play(self):
        self.card_buttons.enable()

    def _start_game(self):
        try:
            self.server.startGame(self.table_id)
        except InvalidOperation, e:
            self.logger.append("Can not start game because: %s" % (e.why))
