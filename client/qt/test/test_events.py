import sys
import unittest

sys.path.append("../../api/target/gen-py")
from tarabish.qt.events import *
#from events import EventDispatcher, Dispatchable, DispatchError
from tarabish.thrift.ttypes import EventType, Event


class EventRegistryTests(unittest.TestCase):
    
    def test_connect_adds_new_type(self):
        dispatcher = EventDispatcher()
        self.assertFalse(EventType.CHAT in dispatcher.event_connections)
        dispatcher.connect(EventType.CHAT, self.a_method)
        self.assertTrue(EventType.CHAT in dispatcher.event_connections)
        
    def test_connect_adds_dispatchable(self):
        dispatcher = EventDispatcher()
        dispatcher.connect(EventType.CHAT, self.a_method)
        self.assertTrue(EventType.CHAT in dispatcher.event_connections)
        self.assertEquals(1, len(dispatcher.event_connections[EventType.CHAT]))      
        self.assertEquals(self.a_method, dispatcher.event_connections[EventType.CHAT][0].method)
        
    def test_connect_adds_multiple_displatchables(self):
        dispatcher = EventDispatcher()
        dispatcher.connect(EventType.CHAT, self.a_method)
        dispatcher.connect(EventType.CHAT, self.b_method)
        self.assertTrue(EventType.CHAT in dispatcher.event_connections)
        self.assertEquals(2, len(dispatcher.event_connections[EventType.CHAT]))      
        self.assertEquals(self.a_method, dispatcher.event_connections[EventType.CHAT][0].method)
        self.assertEquals(self.b_method, dispatcher.event_connections[EventType.CHAT][1].method)

    def test_dispatch_called_on_all_dispatchables(self):
        self.calls = 0
        
        def update_count(type):
            self.calls += 1
        
        dispatcher = EventDispatcher()
        dispatcher.connect(EventType.CHAT, update_count)
        dispatcher.connect(EventType.CHAT, update_count)
        dispatcher.connect(EventType.SIT, update_count)
        
        event = Event(EventType.CHAT)
        dispatcher.dispatch(event)
        self.assertEquals(2, self.calls)
    
    def a_method(self, name, table, message):
        pass
        
    def b_method(self):
        pass
    

class DispatchableTests(unittest.TestCase):
    
    def setUp(self):
        self.message_after_say_message = None
    
    def test_extract_event_props(self):
        dispatchable = Dispatchable(EventType.CHAT, self.say_message)
        self.assertEquals(1, len(dispatchable.event_props))
        self.assertEquals("message", dispatchable.event_props[0])
        
    def test_dispatch_gets_event_values_from_args(self):
        expected_message = "Test Message"
        chat_event = Event(EventType.CHAT, message=expected_message)
        dispatchable = Dispatchable(EventType.CHAT, self.say_message)
        dispatchable.dispatch(chat_event)
        self.assertEquals(expected_message, self.message_after_say_message)
    
    # These tests aren't that strong. Could probably check typed exception or message.
    def test_dispatch_raises_exception_when_incorrect_type(self):
        dispatchable = Dispatchable(EventType.CHAT, self.say_message)
        self.assertRaises(DispatchError, dispatchable.dispatch, Event(EventType.PLAY_CARD))
        
    def test_dispatch_raises_exception_when_event_does_not_have_method_parameter_as_field(self):
        expected_message = "Test Message"
        chat_event = Event(EventType.CHAT, message=expected_message)
        dispatchable = Dispatchable(EventType.CHAT, self.invalid_parameter)
        self.assertRaises(DispatchError, dispatchable.dispatch, chat_event)
    
    def say_message(self, message):
        self.message_after_say_message = message
    
    def invalid_parameter(self, this_param_is_not_an_event_field):
        pass
