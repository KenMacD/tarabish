import inspect

class DispatchError(Exception):
    def __init__(self, message):
        self.message = message

    def __str__(self):
        return self.message

class EventDispatcher(object):
    
    def __init__(self, eventSignal):
        self.event_connections = {}
        eventSignal.connect(self._handle_event)
    
    def connect(self, type, method, table_id=None):
        dispatchable = Dispatchable(type, method, table_id)
        if type in self.event_connections:
            self.event_connections[type].append(dispatchable)
        else:
            self.event_connections[type] = [dispatchable]
    
    def dispatch(self, event):
        if not event.type in self.event_connections:
            return
        
        for dispatchable in self.event_connections[event.type]:
            dispatchable.dispatch(event)

    def _handle_event(self, event):
        self.dispatch(event)


class Dispatchable(object):
    # Args that should not be included from the method sig.
    FILTER_ARG_LIST = ["self"]
    
    def __init__(self, event_type, method, table_id=None):
        self.event_type = event_type
        self.method = method
        self.table_id = table_id
        self.event_props = self._extract_event_props(self.method)
        
    def dispatch(self, event):
        if self.event_type != event.type:
            raise DispatchError("Incorrect event type was dispatched. " + \
                        "Expected %d but was %d" % (self.event_type, event.type))

        # If table_id was specified, do not run the event unless they match
        if self.table_id and getattr(event, "table") != self.table_id:
            return
        
        calling_args = []
        for prop in self.event_props:
            try:
                value = getattr(event, prop)
                calling_args.append(value)
            except AttributeError:
                raise DispatchError("Event does not have attribute %s." % (prop))
        self.method(*tuple(calling_args))
    
    def _extract_event_props(self, method):
        argspec = inspect.getargspec(method)
        return filter(lambda arg: not arg in self.FILTER_ARG_LIST, argspec.args)         
    