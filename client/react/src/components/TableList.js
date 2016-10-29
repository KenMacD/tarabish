import React, { Component, PropTypes } from 'react';
import { connect } from 'react-redux'
import { send } from '../actions'

class TableList extends Component {

  static propTypes = {
    sendMsg: PropTypes.func.isRequired,
    tableList: PropTypes.array.isRequired,
  };

  constructor(props) {
    super(props);
    this.props.sendMsg("get_tables")
  }

  render() {
    let rows = []
    for (let table of this.props.tableList) {
      let seat_elements = []
      for (let seat of table.seats) {
        if (!seat.isOpen) {
          seat_elements.push(<td key={seat.num}>{seat.name}</td>)
        } else {
          seat_elements.push(
            <td key={seat.num}><button onClick={() =>
              this.props.sendMsg("sit", {table_id: table.tableId,
                              seat: seat.num})
            }> Sit </button></td>)
        }
      }
      rows.push(
        <tr key={table.tableId}>
          <td>{table.tableId}</td>
          {seat_elements}
        </tr>
      )
    }
    return (
      <table>
        <thead>
          <tr>
            <th>#</th>
            <th>Seat 1</th>
            <th>Seat 2</th>
            <th>Seat 3</th>
            <th>Seat 4</th>
          </tr>
        </thead>
        <tbody>
          {rows}
        </tbody>
      </table>
    )
  }
}

const mapStateToProps = (state) => {
  const { lobby: {tableList} } = state
  return {
    tableList
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    sendMsg: (method, data = {}) => {
      dispatch(send(method, data))
    }
  }
}

export default connect(mapStateToProps, mapDispatchToProps)(TableList)
