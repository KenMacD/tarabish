import sys
from functools import partial

sys.path.append('api/target/gen-py')
from tarabish.thrift import Tarabish
from tarabish.thrift.ttypes import *
from events import EventDispatcher

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

from PySide.QtCore import (QObject, QThread, QTimer, Signal)

CLIENT_PROTO_VERSION = 2

class ServerEvents(QThread):
    eventSignal = Signal(Event)

    def __init__(self, parent=None):
        super(ServerEvents, self).__init__(parent)
        self.stopped = False

    def initialize(self, host, cookie):
        self.transport = TTransport.TBufferedTransport(TSocket.TSocket(host, 42745))
        protocol = TBinaryProtocol.TBinaryProtocol(self.transport)
        self.eclient = Tarabish.Client(protocol)
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
            while not self.stopped:
                events = self.eclient.getEventsTimeout(self.cookie, 1000)
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
        self.eventDispatcher = EventDispatcher(self.serverEvents.eventSignal)
        self.cookie = 0

        app.aboutToQuit.connect(lambda: self.disconnectFromServer(False))

    def connectToServer(self, host, name):
        if not name or not host:
            raise Exception("Needs name and host")

        try:
            self.transport = TTransport.TBufferedTransport(TSocket.TSocket(host, 42745))
            self.is_connected = True
            protocol = TBinaryProtocol.TBinaryProtocol(self.transport)
            self.client = Tarabish.Client(protocol)
            self.transport.open()

            version = self.client.getVersion()
            if version != CLIENT_PROTO_VERSION:
                raise Exception("Invalid Version") # TODO: better exceptions

            self.cookie = self.client.login(name, "password")
            self.serverEvents.initialize(host, self.cookie)
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
                self.client.quit(self.cookie)
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
            fun = getattr(self.client, attr)
            pfun = partial(fun, self.cookie)

            return pfun
        else:
            return lambda * args, **kwargs: not_connected()
