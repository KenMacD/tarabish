import React, { Component } from 'react';
import './App.css';
import Table from './Table';

class App extends Component {
  render() {
    return (
      <div>
       <div className="App">
          <div className="App-header">
            <h2>Welcome to Tarabish Online</h2>
          </div>
          <div><Table name="Test Table 1"/></div>
        </div>
      </div>
    );
  }
}

export default App;
