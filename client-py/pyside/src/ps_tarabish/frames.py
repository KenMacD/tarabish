from PySide import QtCore
from PySide.QtGui import *
from widgets import TablesTable, ChatWidget
from tarabish.ttypes import InvalidOperation

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
            self.logger.append("<b>Failed: %s</b>" % (str(exc)))


class Table(QDialog):
    def __init__(self, tableId, eventSignal, parent=None):
        super(Table, self).__init__(parent)

        self.setWindowTitle("Tarabish Table %d"%(tableId))
        self.resize(800, 600)

        hbox = QHBoxLayout()
        hbox.addWidget(ChatWidget())

        vbox = QVBoxLayout()
        vbox.addLayout(hbox)

        self.setLayout(vbox)

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
        line.setFrameStyle(QFrame.HLine | QFrame.Sunken)

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
        self.logger.append("Joining table %d, seat %d" % (tableSeatCell.tableId,
            tableSeatCell.seat))

        try:
            self.server.sit(tableSeatCell.tableId, tableSeatCell.seat)
            table = Table(tableSeatCell.tableId, self.server.eventSignal, self)
            self.tables.append(table)
            table.show()
        except InvalidOperation as exc:
            self.logger.append("<b>Failed: %s</b>" % (str(exc)))
