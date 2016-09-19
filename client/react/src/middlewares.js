import { WS_CONNECT, WS_SEND,
  connecting, connected, disconnected,
  messageReceived } from './actions'

const socketMiddleware = store => {
  let socket = null;
  let ws_url = process.env.REACT_APP_WS_URL
  if (!ws_url) {
    const proto = (location.protocol === "https:")? "wss://" : "ws://"
    ws_url = proto + location.host + "/websocket"
  }

  const onOpen = (ws) => evt => {
    store.dispatch(connected());
  }

  const onClose = (ws) => evt => {
    store.dispatch(disconnected());
  }

  const onMessage = (ws) => ({data}) => {
    let msg = null
    try {
      msg = JSON.parse(data);
      console.log("Received data: '" + data + "'");
    } catch (e) {
      console.log("Received non-json: '" + data + "'");
      return
    }
    if (msg) {
      store.dispatch(messageReceived(msg));
    }
  }

  return next => action => {
    const connect = () => {
      if(socket != null) {
        socket.close();
      }
      socket = new WebSocket(ws_url);
      socket.onmessage = onMessage(socket);
      socket.onclose = onClose(socket);
      socket.onopen = onOpen(socket);
    }

    switch(action.type) {
      case WS_CONNECT:
        store.dispatch(connecting());
        connect()
        break;

      case WS_SEND:
        socket.send(JSON.stringify(action.data));
        break;

      default:
        return next(action);
    }
  }
}

export default socketMiddleware
