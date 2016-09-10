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
  };

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
    return (
      <div>
       <div className="App">
          <div className="App-header">
            <h2>Welcome to Tarabish Online</h2>
          </div>
          <div>
            <input type="text" placeholder="Name"
              ref={(ref) => this.nameInput = ref}
            />
            <input type="button" value="Login" onClick={this.login} />
          </div>
          <div><Table name="Test Table 1"/></div>
        </div>
      </div>
    );
  }
}

export default App;
