import React, { Component, PropTypes } from 'react';
import Card from './Card';

export default class Table extends Component {
    static propTypes = {
        name: PropTypes.string.isRequired,
    };

    constructor(props) {
      super(props);
      this.state = {
        direction: "north",
        offset: 0
      };
      this.directions = [ "north", "east", "south", "west"];
    };

    turn_test = (event) => {

      const new_offset = (this.state.offset + 1) % 4;
      this.setState({
        direction: this.directions[new_offset],
        offset: new_offset
      })
    };

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
