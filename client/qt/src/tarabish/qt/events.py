import inspect

class DispatchError(Exception):
    def __init__(self, message):
        self.message = message

    def __str__(self):
        return self.message

class EventDispatcher(object):
    
    def __init__(self, eventSignal):
        self.event_connections = {}
        eventSignal.connect(self.handle_event)
    
    def connect(self, type, method):
        if type in self.event_connections:
            self.event_connections[type].append(Dispatchable(type, method))
        else:
            self.event_connections[type] = [Dispatchable(type, method)]
    
    def dispatch(self, event):
        if not event.type in self.event_connections:
            return
        
        for dispatchable in self.event_connections[event.type]:
            dispatchable.dispatch(event)

    def handle_event(self, event):
        self.dispatch(event)


class Dispatchable(object):
    # Args that should not be included from the method sig.
    FILTER_ARG_LIST = ["self"]
    
    def __init__(self, event_type, method):
        self.event_type = event_type
        self.method = method
        self.event_props = self._extract_event_props(self.method)
        
    def dispatch(self, event):
        if self.event_type != event.type:
            raise DispatchError("Incorrect event type was dispatched. Expected %d but was %d" % (self.event_type, event.type))
        
        calling_args = []
        for prop in self.event_props:
            if not hasattr(event, prop):
                raise DispatchError("Event does not have attribute %s." % (prop))
            value = getattr(event, prop)
            calling_args.append(value)
        
        self.method(*tuple(calling_args))
    
    def _extract_event_props(self, method):
        argspec = inspect.getargspec(method)
        return filter(lambda arg: not arg in self.FILTER_ARG_LIST, argspec.args)
    
