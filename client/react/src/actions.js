

export const WS_CONNECT = 'WS_CONNECT'
export const connect = (url) => {
  return {
    type: WS_CONNECT,
    url: url
  }
}

export const WS_CONNECTED = 'WS_CONNECTED'
export const connected = () => {
  return {
    type: WS_CONNECTED
  }
}

export const WS_CONNECTING = 'WS_CONNECTING'
export const connecting = () => {
  return {
    type: WS_CONNECTING
  }
}

export const WS_DISCONNECTED = 'WS_DISCONNECTED'
export const disconnected = () => {
  return {
    type: WS_DISCONNECTED
  }
}

export const WS_MSG = 'WS_MESSAGE_RECEIVED'
export const messageReceived = (msg) => {
  return {
    type: WS_MSG,
    msg: msg
  }
}

// TODO: make more specific actions?
export const WS_SEND = 'WS_SEND'
export const send = (method, data) => {
  return {
    type: WS_SEND,
    data: {...data, method: method}
  }
}

