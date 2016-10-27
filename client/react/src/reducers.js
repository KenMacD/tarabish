import { combineReducers } from 'redux'
import { WS_MSG } from './actions'


// TODO: Should just one reducer accept WS_MSG
// and repack the message and resend them?

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

function tables(state = {tableList: []}, action) {
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
  auth,
  tables
})

export default tarabishApp
