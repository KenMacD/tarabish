# HACK: Add the module path if run directly
if __name__ == '__main__':
    import sys
    import os

    our_path = os.path.dirname(__file__)
    if not our_path:
        our_path = "."
    top_level = our_path + "/../../../"
    sys.path.insert(0, top_level)

from functools import partial
from tarabish.thrift.constants import (HEARTS, DIAMONDS, SPADES, CLUBS, PASS)
from tarabish.qt.cardlib import PipSource
from PySide.QtGui import (QWidget, QGridLayout, QLabel, QPushButton)

from PySide.QtCore import (Signal, QSize)

class SuitButton(QWidget):
    clicked = Signal(int)
    def __init__(self, resource_path, suit, parent=None):
        super(SuitButton, self).__init__(parent)

        self.suit = suit

        pip_source = PipSource(resource_path)
        pip_pixels = pip_source.get_pip(suit)

        self.img = QLabel(self)
        self.img.setPixmap(pip_pixels)
        self.img.setFixedSize(50, 50)
        self.img.move(0, 0)

    def sizeHint(self):
        return self.minimumSizeHint()

    def minimumSizeHint(self):
        return QSize(50, 50)

    def mousePressEvent(self, event):
        self.clicked.emit(self.suit)

class TrumpWidget(QWidget):
    selected = Signal(int)

    def __init__(self, resource_path, parent=None):
        super(TrumpWidget, self).__init__(parent)

        self.trump_layout = QGridLayout()

        self.hearts   = SuitButton(resource_path, HEARTS)
        self.spades   = SuitButton(resource_path, SPADES)
        self.clubs    = SuitButton(resource_path, CLUBS)
        self.diamonds = SuitButton(resource_path, DIAMONDS)

        self.hearts.clicked.connect(self._selected)
        self.spades.clicked.connect(self._selected)
        self.clubs.clicked.connect(self._selected)
        self.diamonds.clicked.connect(self._selected)

        self.ipass    = QPushButton("Pass")
        self.ipass.clicked.connect(partial(self._selected, PASS))

        self.trump_layout.addWidget(self.hearts,   0, 0)
        self.trump_layout.addWidget(self.spades,   0, 1)
        self.trump_layout.addWidget(self.clubs,    1, 0)
        self.trump_layout.addWidget(self.diamonds, 1, 1)
        self.trump_layout.addWidget(self.ipass,    2, 0, 1, 2)

        self.setLayout(self.trump_layout)
        self.setFixedSize(150, 184)

    def sizeHint(self):
        return self.minimumSizeHint()

    def minimumSizeHint(self):
        return QSize(150, 184)

    def _selected(self, suit):
        self.selected.emit(suit)

if __name__ == '__main__':
    def print_suit(suit):
        print "Selected suit %d"%(suit)

    from PySide.QtGui import QApplication
    
    app = QApplication(sys.argv)

    widget = TrumpWidget(top_level + "../resources")
    widget._selected = print_suit
    widget.show()
    widget.raise_()
    app.exec_()
