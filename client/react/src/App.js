import React, { Component } from 'react';
import './App.css';
import Table from './Table';


const TableList = ({table_data}) => {
  let items = []
  for (let table of table_data) {
    items.push(<p key={table.tableId}>This is table {table.tableId}</p>)
  }
  return (
    <ul>{items}</ul>
  )
}

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
      show_login: true,
      table_data: []
    }
  };

  sendMessage = (data) => {
    this.ws.send(JSON.stringify(data))
  }

  handleMessage = (data) => {
    console.log("Received data: " + data)
    let msg = null;
    try {
      msg = JSON.parse(data)
    } catch (e) {
      console.log("Error: Non JSON data")
      return
    }
    switch (msg.type) {
      case "valid_login":
        console.log("Logged In")
        this.setState({show_login: false})
        break
      case "tables":
        console.log("Received Tables")
        this.setState({table_data: msg.tables})
        break
      default:
        console.log("Unknown msg type " + msg.type)
    }
  }

  login = (event) => {
    let name = this.nameInput.value
    if (!name) {
      console.log("No name supplied");
      return
    }
    console.log("Logging in as " + name);
    this.sendMessage({
      method: "login",
      name: name
    })
  }

  get_tables = (event) => {
    console.log("Getting tables")
    this.sendMessage({method: "get_tables"})
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
          <TableList table_data={this.state.table_data} />
          <a href="#" onClick={this.get_tables}>Get Tables</a>
          <div><Table name="Test Table 1"/></div>
        </div>
      </div>
    );
  }
}

export default App;
