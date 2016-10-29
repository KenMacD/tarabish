import { combineReducers } from 'redux'
import { WS_MSG } from './actions'


// TODO: Should just one reducer accept WS_MSG
// and repack the message and resend them?

function screen(state = {name: "login"}, action) {
  switch (action.type) {
    case WS_MSG:
      let msg = action.msg
      switch (msg.type) {
        case "valid_login":
          return {
            ...state,
            name: "lobby",
          }
        default:
          break
      }
      break
    default:
      break
  }
  return state
 }

function auth(state = {}, action) {
  switch (action.type) {
    case WS_MSG:
      let msg = action.msg
      switch (msg.type) {
        case "valid_login":
          return {
            ...state,
            name: msg.name
          }
        default:
          break
      }
      break
    default:
      break
  }
  return state
}

function lobby(state = {tableList: []}, action) {
  switch (action.type) {
    // get-tables, sit, stand...
    case WS_MSG:
      let msg = action.msg
      switch (msg.type) {
        case "tables":
          return {
            ...state,
            tableList: msg.tables,
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
  screen,
  auth,
  lobby,
})

export default tarabishApp
