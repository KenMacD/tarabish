import sys

sys.path.append('api/target/gen-py')
from tarabish import Tarabish, TarabishMsg
from tarabish.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from PyQt4 import QtCore
from PyQt4.QtCore import (Qt, QObject, QThread, QTimer, SIGNAL)
# from PyQt4.QtGui import (QApplication, QFrame, QLabel, QDialog, QLineEdit, QTextBrowser,
#                QVBoxLayout, QHBoxLayout, QGridLayout)
from PyQt4.QtGui import *

CLIENT_PROTO_VERSION = 1

class ServerEvents(QThread):
    def __init__(self, parent=None):
        super(ServerEvents, self).__init__(parent)
        self.stopped = False

    def initialize(self, host, cookie):
        self.transport = TTransport.TBufferedTransport(TSocket.TSocket(host, 42746))
        protocol = TBinaryProtocol.TBinaryProtocol(self.transport)
        self.eclient = TarabishMsg.Client(protocol)
        self.cookie = cookie
        self.stopped = False

    def stop(self):
        self.stopped = True

    def run(self):
        if not self.eclient:
            self.stop()
            return

        try:
            self.transport.open()
            self.eclient.login(self.cookie)
            while not self.stopped:
                events = self.eclient.getEventsTimeout(1000)
                if not events:
                    continue
                self.emit(SIGNAL("events(PyQt_PyObject)"), events)
        except Exception as exc:
            print "Exception from eclient: " + str(exc)
        finally:
            self.transport.close()
            del self.eclient

        self.emit(SIGNAL("finished()"))

class ServerConnection(QObject):
    def __init__(self, app, serverEvents, parent=None):
        super(ServerConnection, self).__init__(parent)

        self.connected = False
        self.hasEvents = False
        self.serverEvents = serverEvents

        app.aboutToQuit.connect(lambda: self.disconnect(False))

    def connect(self, host, name):
        if not name or not host:
            raise Exception("Needs name and host")

        try:
            self.transport = TTransport.TBufferedTransport(TSocket.TSocket(host, 42745))
            self.connected = True
            protocol = TBinaryProtocol.TBinaryProtocol(self.transport)
            self.client  = Tarabish.Client(protocol)
            self.transport.open()

            version = self.client.getVersion()
            if version != CLIENT_PROTO_VERSION:
                raise Exception("Invalid Version") # TODO: better exceptions

            cookie = self.client.login(name, "password")
            self.serverEvents.initialize(host, cookie)
            self.serverEvents.start()
            self.hasEvents = True

            self.emit(SIGNAL("connected()"))

        except:
            # Close possibly half-open connections
            self.disconnect(notify=False)
            raise

    def disconnect(self, notify=True):
        if self.connected:
            try:
                self.client.quit()
            except InvalidOperation:
                pass
            self.serverEvents.stop()
            self.transport.close()
            self.connected = False

            if notify:
                self.emit(SIGNAL("disconnected()"))

    def getTables(self):
        if not self.connected:
            return []
        return self.client.getTables()

class LoginFrame(QFrame):
    def __init__(self, server, logger, parent=None):
        super(LoginFrame, self).__init__(parent)
        self.logger = logger
        self.server = server
        self.connected = False

        layout = QGridLayout()

        layout.addWidget(QLabel("Host: "), 0, 0)

        self.host = QLineEdit("localhost")
        layout.addWidget(self.host, 0, 1)

        layout.addWidget(QLabel("Name: "), 1, 0)
        self.name = QLineEdit()
        layout.addWidget(self.name, 1, 1)

        self.connectButton = QPushButton("Connect")
        layout.addWidget(self.connectButton, 2, 0, 1, 2)

        self.setLayout(layout)

        self.connect(self.connectButton, SIGNAL("clicked()"),
                self.pressConnectButton)

        self.connect(self.server, SIGNAL("connected()"),
                self.handleConnected)

        self.connect(self.server, SIGNAL("disconnected()"),
                self.handleDisconnected)

    def handleConnected(self):
        self.logger.append("<b>Connected</b>")
        self.connected = True
        self.connectButton.setText("Disconnect")

    def handleDisconnected(self):
        self.logger.append("<b>Disconnected</b>")
        self.connected = False
        self.connectButton.setText("Connect")

    def pressConnectButton(self):
        try:
            if self.connected:
                self.server.disconnect()
            else:
                host = unicode(self.host.text())
                name = unicode(self.name.text())
                self.server.connect(host, name)
        except Exception as exc: # TODO: better exception
            self.logger.append("<b>Failed: %s</b>"%(str(exc)))

class TableListFrame(QFrame):
    def __init__(self, server, logger, parent=None):
        super(TableListFrame, self).__init__(parent)

        self.server = server
        self.logger = logger
        self.timer = QTimer()

        self.tables = QTableWidget()
        layout = QVBoxLayout()
        layout.addWidget(self.tables)
        self.setLayout(layout)

        self.connect(self.server, SIGNAL("connected()"),
                self.startUpdating)
        self.connect(self.server, SIGNAL("disconnected()"),
                self.stopUpdating)
        self.connect(self.timer, SIGNAL("timeout()"), self.updating)

    def startUpdating(self):
        self.logger.append("Start Updating")
        self.timer.start(5000)
        self.updating()

    def updating(self):
        tableList = self.server.getTables()

        self.tables.clear()
        self.tables.setRowCount(len(tableList))
        self.tables.setColumnCount(5)
        self.tables.setHorizontalHeaderLabels(
                ["Table", "Seat 1", "Seat 2", "Seat 3", "Seat 4"])
        self.tables.setAlternatingRowColors(True)
        self.tables.verticalHeader().hide()

        for row, table in enumerate(tableList):
            item = QTableWidgetItem(str(table.tableId))
            item.setFlags(QtCore.Qt.NoItemFlags)
            item.setTextAlignment(QtCore.Qt.AlignCenter)
            self.tables.setItem(row, 0, item)

            for col, seat in enumerate(table.seats):
                item = QTableWidgetItem(seat.name)
                item.setFlags(QtCore.Qt.NoItemFlags)
                self.tables.setItem(row, col + 1, item)
#        self.tables.resizeColumnsToContents()

    def stopUpdating(self):
        self.logger.append("Stop Updating")
        self.timer.stop()

class MainForm(QDialog):

    def __init__(self, server, parent=None):
        super(MainForm, self).__init__(parent)

        self.log = QTextBrowser()
        self.log.append("Event Log")

        self.login = LoginFrame(server, self.log)

        line = QFrame()
        line.setFrameStyle(QFrame.HLine|QFrame.Sunken)

        tables = TableListFrame(server, self.log)

        layout = QVBoxLayout()
        layout.addWidget(self.login)
        layout.addWidget(line)
        layout.addWidget(tables)
        layout.addWidget(self.log)
        self.setLayout(layout)

        self.setWindowTitle("Tarabish Test Client")


# TODO: handle SIGINT, ctrl+c then closing the window doesn't disconnect
app = QApplication(sys.argv)

serverEvents = ServerEvents()
server = ServerConnection(app, serverEvents)
main = MainForm(server)
main.resize(400, 600)
main.show()
main.raise_()
app.exec_()
