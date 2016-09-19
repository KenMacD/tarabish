import { combineReducers } from 'redux'
import { WS_MSG } from './actions'

function auth(state = {isAuthenticated: false}, action) {
  switch (action.type) {
    case WS_MSG:
      let msg = action.msg
      switch (msg.type) {
        case "valid_login":
          return {
            ...state,
            isAuthenticated: true
          }
        default:
          break
      }
      break
    default:
      break
  }
  return state;
}

const tarabishApp = combineReducers({
  auth
})

export default tarabishApp
