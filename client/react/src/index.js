// React
import React from 'react';
import { render } from 'react-dom';

// Redux
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import tarabishApp from './reducers'
import App from './containers/App'

// WebSocket Middleware
import socketMiddleware from './middlewares'

import './index.css';


let store = createStore(tarabishApp, applyMiddleware(socketMiddleware))

// TODO: connect this during login, if waiting on connection is working.
import { connect } from './actions'
store.dispatch(connect())

render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root')
)
