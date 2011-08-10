import sys

sys.path.append('api/target/gen-py')
from tarabish import Tarabish, TarabishMsg
from tarabish.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from PyQt4.QtCore import (Qt, SIGNAL)
# from PyQt4.QtGui import (QApplication, QFrame, QLabel, QDialog, QLineEdit, QTextBrowser,
#                QVBoxLayout, QHBoxLayout, QGridLayout)
from PyQt4.QtGui import *

class LoginFrame(QFrame):
    def __init__(self, parent=None):
        super(LoginFrame, self).__init__(parent)

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
                self.connectToServer)

    def connectToServer(self):
        host = unicode(self.host.text())
        name = unicode(self.name.text())

        if not name or not host:
            return

        try:
            t1 = TTransport.TBufferedTransport(TSocket.TSocket(host, 42745))
            t2 = TTransport.TBufferedTransport(TSocket.TSocket(host, 42746))

            p1 = TBinaryProtocol.TBinaryProtocol(t1)
            p2 = TBinaryProtocol.TBinaryProtocol(t2)

            self.client  = Tarabish.Client(p1)
            self.eclient = TarabishMsg.Client(p2)

            t1.open()
            t2.open()

        except Exception as exc: # TODO: better exception

            import traceback
            traceback.print_exc(exc)

            if t1:
                t1.close()
            if t2:
                t2.close()
            return

        # TODO: change action
        self.connectButton.setText("Disconnect")


class MainForm(QDialog):

    def __init__(self, parent=None):
        super(MainForm, self).__init__(parent)

        self.log = QTextBrowser()
        self.log.append("Event Log")

        self.login = LoginFrame()

        layout = QVBoxLayout()
        layout.addWidget(self.login)
        layout.addWidget(self.log)
        self.setLayout(layout)

        self.setWindowTitle("Tarabish Test Client")


app = QApplication(sys.argv)
main = MainForm()
main.show()
main.raise_()
app.exec_()
