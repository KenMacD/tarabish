from PySide.QtGui import *

class ChatWidget(QWidget):
    def __init__(self):
        super(ChatWidget, self).__init__()
        send_button = QPushButton("Send")
        self.message_box = QLineEdit()
        self.messages = QTextBrowser()

        chat_widgets = QHBoxLayout()
        chat_widgets.addWidget(self.message_box)
        chat_widgets.addWidget(send_button)

        chat_container = QVBoxLayout()
        chat_container.addWidget(self.messages)
        chat_container.addLayout(chat_widgets)
        self.setLayout(chat_container)

        send_button.clicked.connect(self._send_message)

    def _send_message(self):
        # TODO [mstead] Send message via server
        self.messages.append(self.message_box.text())
        self.message_box.clear()

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
