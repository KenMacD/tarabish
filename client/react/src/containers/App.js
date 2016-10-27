import React, { Component, PropTypes } from 'react'
import './App.css'
import { connect } from 'react-redux'
import { send } from '../actions'

import Login from '../components/Login'
import TableList from '../components/TableList'

class App extends Component {
  render() {
    const { sendMsg, isAuthenticated } = this.props
    return (
      <div>
        <div className="App">
          <div className="App-header">
            <h2>Welcome to Tarabish Online</h2>
          </div>

          {!isAuthenticated? (
              <Login sendMsg={sendMsg} />
            ) : (
              <TableList/>
            )
          }
        </div>
      </div>
    )
  }
}

App.propTypes = {
  sendMsg: PropTypes.func.isRequired,
  isAuthenticated: PropTypes.bool.isRequired,
}

function mapStateToProps(state) {

  const { auth: { isAuthenticated } } = state

  return {
    isAuthenticated,
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    sendMsg: (method, data) => {
      dispatch(send(method, data))
    }
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(App)
