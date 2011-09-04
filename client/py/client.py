#!/usr/bin/env python

import sys
#sys.path.append('target/generated-sources/gen-py')

from tarabish import Tarabish
from tarabish.ttypes import *

from thrift import Thrift
from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol

# Make socket
transport = TSocket.TSocket('localhost', 42745)

# Buffering is critical. Raw sockets are very slow
transport = TTransport.TBufferedTransport(transport)

# Wrap in a protocol
protocol = TBinaryProtocol.TBinaryProtocol(transport)

# Create a client to use the protocol encoder
client = Tarabish.Client(protocol)

# Connect!
transport.open()
 
#print "Get Version: " + str(client.getVersion())
#print "Create Alice: " + str(client.createAccount("Alice", "a@invalid", "Password"))
#print "Login as Alice wrong pw: " + str(client.login("Alice", "PasswordWrong"))
#print "Login as Alice: " + str(client.login("Alice", "Password"))
