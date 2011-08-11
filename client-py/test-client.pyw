import sys

sys.path.append('api/target/gen-py')
from tarabish import Tarabish, TarabishMsg
from tarabish.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from PyQt4.QtCore import (Qt, QObject, QThread, SIGNAL)
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
    def __init__(self, serverEvents, parent=None):
        super(ServerConnection, self).__init__(parent)

        self.connected = False
        self.hasEvents = False
        self.serverEvents = serverEvents

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
            self.serverEvents.stop()
            self.transport.close()
            self.connected = False

            if notify:
                self.emit(SIGNAL("disconnected()"))


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

class MainForm(QDialog):

    def __init__(self, server, parent=None):
        super(MainForm, self).__init__(parent)

        self.log = QTextBrowser()
        self.log.append("Event Log")

        line = QFrame()
        line.setFrameStyle(QFrame.HLine|QFrame.Sunken)

        self.login = LoginFrame(server, self.log)

        layout = QVBoxLayout()
        layout.addWidget(self.login)
        layout.addWidget(line)
        layout.addWidget(self.log)
        self.setLayout(layout)

        self.setWindowTitle("Tarabish Test Client")


app = QApplication(sys.argv)

serverEvents = ServerEvents()
server = ServerConnection(serverEvents)
main = MainForm(server)
main.show()
main.raise_()
app.exec_()
