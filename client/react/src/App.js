import React, { Component } from 'react';
import './App.css';
import Table from './Table';

class App extends Component {
  constructor () {
    super();
    const ws = new WebSocket('ws://localhost:42745/websocket')
    this.state = {
      ws: ws
    }
    ws.onopen = () => console.log("OPENED")
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
    this.state.ws.send(JSON.stringify(msg))
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
