
from tarabish.thrift.constants import (CLUBS, SPADES, HEARTS, DIAMONDS)
from tarabish.thrift.constants import (JACK, QUEEN, KING, ACE)

from PySide import QtGui
from PySide.QtSvg import QSvgRenderer

__suit_to_char = {CLUBS:   'C',
                 HEARTS:   'H',
                 SPADES:   'S',
                 DIAMONDS: 'D'}

__value_to_char = {JACK:  'J',
                   QUEEN: 'Q',
                   KING:  'K',
                   ACE:   'A'}

CARD_WIDTH  = 64
CARD_HEIGHT = 90

_renderer = None

def _get_renderer(path):
    global _renderer
    if not _renderer:
        _renderer = QSvgRenderer(path + "/cards.svg")
    return _renderer

def get_card(path, value, suit):
    pix = QtGui.QPixmap(CARD_WIDTH, CARD_HEIGHT)
    painter = QtGui.QPainter(pix)
    renderer = _get_renderer(path)

    if value > 10:
        id = __value_to_char[value] + __suit_to_char[suit]
    else:
        id = str(value) + __suit_to_char[suit]

    renderer.render(painter, id)
    return pix
