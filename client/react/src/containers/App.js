import React, { Component, PropTypes } from 'react'
import './App.css'
import { connect } from 'react-redux'
import { send } from '../actions'

import Login from '../components/Login'
import TableList from '../components/TableList'
import Table from '../components/Table'

class App extends Component {
  render() {
    const { sendMsg, screenName } = this.props
    const screens = {
      "login": () => <Login sendMsg={sendMsg} />,
      "lobby": () => <TableList/>,
      "table": () => <Table name={"TESTING"}/>,
    }
    const Screen = screens[screenName]
    return (
      <div>
        <div className="App">
          <div className="App-header">
            <h2>Welcome to Tarabish Online</h2>
          </div>
          <Screen/>
        </div>
      </div>
    )
  }
}

App.propTypes = {
  sendMsg: PropTypes.func.isRequired,
  screenName: PropTypes.string.isRequired,
}

function mapStateToProps(state) {

  const { screen: { name: screenName } } = state

  return {
    screenName,
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
