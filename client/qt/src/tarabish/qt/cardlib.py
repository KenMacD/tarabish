
from tarabish.thrift.constants import (CLUBS, SPADES, HEARTS, DIAMONDS)
from tarabish.thrift.constants import (JACK, QUEEN, KING, ACE)

from PySide.QtCore import Qt
from PySide import QtGui
from PySide.QtSvg import QSvgRenderer

_suit_to_char = {CLUBS:   'C',
                 HEARTS:   'H',
                 SPADES:   'S',
                 DIAMONDS: 'D'}

_value_to_char = {JACK:  'J',
                   QUEEN: 'Q',
                   KING:  'K',
                   ACE:   'A'}

CARD_WIDTH  = 64
CARD_HEIGHT = 90

PIP_WIDTH  = 50
PIP_HEIGHT = 50

__sources = {}

def _get_renderer(path):
    global __sources
    if not path in __sources:
        renderer = QSvgRenderer(path)
        __sources[path] = renderer
    return __sources[path]

class CardSource(object):
    def __init__(self, path):
        global _get_renderer
        self.renderer = _get_renderer(path + "/cards.svg")

    def get_card(self, value, suit):
        pix = QtGui.QPixmap(CARD_WIDTH, CARD_HEIGHT)
        pix.fill(Qt.transparent)
        painter = QtGui.QPainter(pix)

        if value > 10:
            id = _value_to_char[value] + _suit_to_char[suit]
        else:
            id = str(value) + _suit_to_char[suit]

        self.renderer.render(painter, id)
        return pix

class PipSource(object):
    suit_to_path = {HEARTS:   "path143",
                    CLUBS:    "path147",
                    SPADES:   "path153",
                    DIAMONDS: "path155"}

    def __init__(self, path):
        global _get_renderer
        self.renderer = _get_renderer(path + "/pips.svg")

    def get_pip(self, suit):
        path = PipSource.suit_to_path[suit]
        pix = QtGui.QPixmap(PIP_WIDTH, PIP_HEIGHT)
        pix.fill(Qt.transparent)
        painter = QtGui.QPainter(pix)

        self.renderer.render(painter, path)
        return pix
