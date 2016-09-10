import React, { Component } from 'react';
import './App.css';
import Table from './Table';


class App extends Component {
  constructor () {
    super();

    let ws_url = process.env.REACT_APP_WS_URL
    if (!ws_url) {
      const proto = (location.protocol === "https:")? "wss://" : "ws://"
      ws_url = proto + location.host + "/websocket"
    }
    this.ws = new WebSocket(ws_url)
    this.ws.onopen = () => console.log("OPENED")

    this.ws.onmessage = ({data}) => this.handleMessage(data)

    this.state = {
      show_login: true
    }
  };

  handleMessage = (data) => {
    console.log("Received data: " + data)
    let msg = null;
    try {
      msg = JSON.parse(data)
    } catch (e) {
      console.log("Error: Non JSON data")
      return
    }
    if (msg.type === "valid_login") {
      console.log("Logged In")
      this.setState({show_login: false})
    }
  }

  login = (event) => {
    let name = this.nameInput.value
    if (!name) {
      console.log("No name supplied");
      return
    }
    console.log("Logging in as " + name);
    let msg = {
      method: "login",
      name: name
    }
    this.ws.send(JSON.stringify(msg))
  }

  render() {
    let login_block = (
      <div>
        <input type="text" placeholder="Name"
          ref={(ref) => this.nameInput = ref}
        />
        <input type="button" value="Login" onClick={this.login} />
      </div>
    )
    if (!this.state.show_login) {
      login_block = null
    }
    return (
      <div>
       <div className="App">
          <div className="App-header">
            <h2>Welcome to Tarabish Online</h2>
          </div>
          {login_block}
          <div><Table name="Test Table 1"/></div>
        </div>
      </div>
    );
  }
}

export default App;
