#!/usr/bin/env python

import sys

from PySide.QtGui import QApplication
from tarabish.qt.connection import ServerEvents, ServerConnection
from tarabish.qt.frames import MainForm

print "Running tarabish..."

# TODO: handle SIGINT, ctrl+c then closing the window doesn't disconnect
app = QApplication(sys.argv)

serverEvents = ServerEvents()
server = ServerConnection(app, serverEvents)
main = MainForm(server)
main.resize(600, 600)
main.show()
main.raise_()
app.exec_()
