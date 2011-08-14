import sys

sys.path.append('api/target/gen-py')
from tarabish import Tarabish, TarabishMsg
from tarabish.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from PySide import QtCore
from PySide.QtCore import (QObject, QThread, QTimer, Signal)
from PySide.QtGui import *

CLIENT_PROTO_VERSION = 1

class ServerEvents(QThread):
    eventSignal = Signal(Event)

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
                for event in events:
                    self.eventSignal.emit(event)
        except InvalidOperation:
            pass # Client down.
        except Exception as exc:
            print "Exception from eclient: " + str(exc)
        finally:
            self.transport.close()
            del self.eclient

class ServerConnection(QObject):
    connected = Signal()
    disconnected = Signal()

    def __init__(self, app, serverEvents, parent=None):
        super(ServerConnection, self).__init__(parent)

        self.is_connected = False
        self.hasEvents = False
        self.serverEvents = serverEvents
        self.eventSignal = serverEvents.eventSignal

        app.aboutToQuit.connect(lambda: self.disconnectFromServer(False))

    def connectToServer(self, host, name):
        if not name or not host:
            raise Exception("Needs name and host")

        try:
            self.transport = TTransport.TBufferedTransport(TSocket.TSocket(host, 42745))
            self.is_connected = True
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

            self.connected.emit()

        except:
            # Close possibly half-open connections
            self.disconnectFromServer(notify=False)
            raise

    def disconnectFromServer(self, notify=True):
        if self.is_connected:
            try:
                self.client.quit()
            except InvalidOperation:
                pass
            self.serverEvents.stop()
            self.serverEvents.wait()
            self.transport.close()
            self.is_connected = False

            if notify:
                self.disconnected.emit()

    # Handle calls to client.Method()
    def __getattr__(self, attr):
        def not_connected():
            raise InvalidOperation("Not Connected")

        if self.is_connected:
            return getattr(self.client, attr)
        else:
            return lambda *args, **kwargs: not_connected()

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

        self.connectButton.clicked.connect(self.pressConnectButton)

        self.server.connected.connect(self.handleConnected)
        self.server.disconnected.connect(self.handleDisconnected)

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
                self.server.disconnectFromServer()
            else:
                host = unicode(self.host.text())
                name = unicode(self.name.text())
                self.server.connectToServer(host, name)
        except Exception as exc: # TODO: better exception
            self.logger.append("<b>Failed: %s</b>"%(str(exc)))

class TableSeatCell(QTableWidgetItem):
    def __init__(self, string, tableId, seat):
        super(TableSeatCell, self).__init__(string)

        self.tableId = tableId
        self.seat = seat

class TablesTable(QTableWidget):
    def __init__(self, server, logger, refreshButton, parent=None):
        super(TablesTable, self).__init__(parent)

        self.server = server
        self.logger = logger
        self.timer = QTimer()

        self.server.connected.connect(self.updating)
        self.timer.timeout.connect(self.updating)
        refreshButton.clicked.connect(self.updating)

    def startUpdating(self):
        self.logger.append("Start Updating")
        self.timer.start(5000)
        self.updating()

    def updating(self):
        self.logger.append("Updating Tables")
        try:
            tableList = self.server.getTables()
        except InvalidOperation as exc:
            self.logger.append("<b>Failed: %s</b>"%(str(exc)))
            return

        self.clear()
        self.setRowCount(len(tableList))
        self.setColumnCount(5)
        self.setHorizontalHeaderLabels(
                ["Table", "Seat 1", "Seat 2", "Seat 3", "Seat 4"])
        self.verticalHeader().hide()
        self.setAlternatingRowColors(True)
        self.setEditTriggers(QTableWidget.NoEditTriggers)
        self.setSelectionMode(QTableWidget.SingleSelection)
        self.setSelectionBehavior(QTableWidget.SelectItems)

        for row, table in enumerate(tableList):
            item = QTableWidgetItem(str(table.tableId))
            item.setTextAlignment(QtCore.Qt.AlignCenter)
            self.setItem(row, 0, item)

            for col, seat in enumerate(table.seats):
                if not seat.isOpen:
                    item = QTableWidgetItem(seat.name)
                    item.setFlags(QtCore.Qt.NoItemFlags)
                else:
                    item = TableSeatCell("", table.tableId, col)
                self.setItem(row, col + 1, item)

#        self.tables.resizeColumnsToContents()

    def stopUpdating(self):
        self.logger.append("Stop Updating")
        self.timer.stop()

class Table(QDialog):
    def __init__(self, tableId, eventSignal, parent=None):
        super(Table, self).__init__(parent)

        self.setWindowTitle("Tarabish Table %d"%(tableId))
        self.resize(800, 600)
        self.logger = QTextBrowser()

        layout = QVBoxLayout()
        layout.addWidget(QLabel("TESTING"))
        layout.addWidget(self.logger)
        self.setLayout(layout)

        eventSignal.connect(self.handleEvent)

    def handleEvent(self, event):
        self.logger.append("Received Event: " + str(event))


class MainForm(QDialog):

    def __init__(self, server, parent=None):
        super(MainForm, self).__init__(parent)

        self.server = server
        self.tables = []
        self.logger = QTextBrowser()
        self.login = LoginFrame(server, self.logger)

        line = QFrame()
        line.setFrameStyle(QFrame.HLine|QFrame.Sunken)

        tableRefreshButton = QPushButton("Refresh")
        tablesTable = TablesTable(server, self.logger, tableRefreshButton)

        tableLabel = QLabel("Tables:")
        tableLabel.setAlignment(QtCore.Qt.AlignCenter)

        tablesLayout = QVBoxLayout()
        tablesLayout.setContentsMargins(0, 0, 0, 0);
        tablesLayout.addStretch()
        tablesLayout.addWidget(tableLabel)
        tablesLayout.addWidget(tableRefreshButton)
        tablesLayout.addStretch()

        logLabel = QLabel("Log:")
        logLabel.setAlignment(QtCore.Qt.AlignCenter)

        bottomLayout = QGridLayout()
        bottomLayout.setContentsMargins(0, 0, 0, 0);
        bottomLayout.addLayout(tablesLayout, 0, 0)
        bottomLayout.addWidget(tablesTable, 0, 1)
        bottomLayout.addWidget(logLabel, 1, 0)
        bottomLayout.addWidget(self.logger, 1, 1)

        layout = QVBoxLayout()
        layout.addWidget(self.login)
        layout.addWidget(line)
        layout.addLayout(bottomLayout)
        self.setLayout(layout)

        self.setWindowTitle("Tarabish Test Client")

        tablesTable.itemDoubleClicked.connect(self.handleSit)

    def handleSit(self, tableSeatCell):
        self.logger.append("Joining table %d, seat %d"%(tableSeatCell.tableId,
            tableSeatCell.seat))

        try:
            self.server.sit(tableSeatCell.tableId, tableSeatCell.seat)
            table = Table(tableSeatCell.tableId, self.server.eventSignal, self)
            self.tables.append(table)
            table.show()
        except InvalidOperation as exc:
            self.logger.append("<b>Failed: %s</b>"%(str(exc)))


# TODO: handle SIGINT, ctrl+c then closing the window doesn't disconnect
app = QApplication(sys.argv)

serverEvents = ServerEvents()
server = ServerConnection(app, serverEvents)
main = MainForm(server)
main.resize(600, 600)
main.show()
main.raise_()
app.exec_()
