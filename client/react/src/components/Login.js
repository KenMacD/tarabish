import React, { Component } from 'react';

class Login extends Component {

  login = (event) => {
    let name = this.nameInput.value
    if (!name) {
      console.log("No name supplied")
      return
    }
    console.log("Logging in as " + name);
    this.props.sendMsg("login", {name: name})
  }

  render() {
    return (
      <div>
        <input type="text" placeholder="Name"
          ref={(ref) => this.nameInput = ref}
        />
        <input type="button" value="Login" onClick={this.login} />
      </div>
    )
  }
}

export default Login

