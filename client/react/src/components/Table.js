import React, { Component, PropTypes } from 'react';
import Card from './Card';

export default class Table extends Component {
    static propTypes = {
      name: PropTypes.string.isRequired,
      ws: PropTypes.object.isRequired,
    };

    constructor(props) {
      super(props);
      this.state = {
        direction: "north",
        offset: 0
      };
      this.directions = [ "north", "east", "south", "west"];
    };

    componentDidMount() {
      this.props.ws.addEventListener("message", this.handleMessage)
    }

    componentWillUnmount() {
      this.props.ws.removeEventListener("message", this.handleMessage)
    }

    turn_test = (event) => {

      const new_offset = (this.state.offset + 1) % 4;
      this.setState({
        direction: this.directions[new_offset],
        offset: new_offset
      })
    };

    handleMessage = (data) => {
      console.log("Table " + this.props.name + " received: " + data)
    }

    render() {
        return (
          <div>
            <a href='#' onClick={this.turn_test}>Click Test</a>
            <div><Card direction={this.state.direction}/></div>
            <div><Card value={10} suit={1} direction={this.state.direction}/></div>
          </div>
        )
    }
}
