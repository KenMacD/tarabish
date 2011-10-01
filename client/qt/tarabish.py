#!/usr/bin/env python

import sys
import os

our_path = os.path.dirname(__file__)

if sys.frozen:
    resource_path = os.environ["_MEIPASS2"]
else:
    resource_path = os.path.dirname(__file__) + "/resources/"
    sys.path.insert(0,our_path + "/src/")

from PySide.QtGui import QApplication
from tarabish.qt.connection import ServerEvents, ServerConnection
from tarabish.qt.lobby import MainForm

print "Running tarabish..."

# TODO: handle SIGINT, ctrl+c then closing the window doesn't disconnect
app = QApplication(sys.argv)

serverEvents = ServerEvents()
server = ServerConnection(app, serverEvents)
main = MainForm(server, resource_path)
main.resize(600, 600)
main.show()
main.raise_()
app.exec_()
