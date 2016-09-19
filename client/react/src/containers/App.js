import React, { Component, PropTypes } from 'react'
import './App.css'
import { connect } from 'react-redux'
import { send } from '../actions'

import Login from '../components/Login'

class App extends Component {
  render() {
    const { isAuthenticated, sendLogin } = this.props
    return (
      <div>
        <div className="App">
          <div className="App-header">
            <h2>Welcome to Tarabish Online</h2>
          </div>

          {!isAuthenticated? (
            <Login sendLogin={sendLogin} />
            ) : null
          }
        </div>
      </div>
    )
  }
}

App.propTypes = {
  sendLogin: PropTypes.func.isRequired,
  isAuthenticated: PropTypes.bool.isRequired
}

function mapStateToProps(state) {

  const { auth } = state
  const { isAuthenticated } = auth

  return {
    isAuthenticated,
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    sendLogin: (name) => {
      dispatch(send("login", {
        name: name
      }))
    }
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(App)
