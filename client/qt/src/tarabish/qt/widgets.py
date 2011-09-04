from PySide import QtCore
from PySide.QtCore import (QObject, QThread, QTimer, Signal)
from PySide.QtGui import *

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
            self.logger.append("<b>Failed: %s</b>" % (str(exc)))
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