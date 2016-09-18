// React
import React from 'react';
import { render } from 'react-dom';

// Redux
import { Provider } from 'react-redux'
import { createStore } from 'redux'
import tarabishApp from './reducers'
import App from './components/App'

import './index.css';

let store = createStore(tarabishApp)

render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root')
)
