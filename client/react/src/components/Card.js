import React, { PropTypes } from 'react';
import './Card.css';
import back from './images/back.png';

// I really hope there's a better way to do this:
const cards = [
  require('./images/1.png'),
  require('./images/2.png'),
  require('./images/3.png'),
  require('./images/4.png'),
  require('./images/5.png'),
  require('./images/6.png'),
  require('./images/7.png'),
  require('./images/8.png'),
  require('./images/9.png'),
  require('./images/10.png'),
  require('./images/11.png'),
  require('./images/12.png'),
  require('./images/13.png'),
  require('./images/14.png'),
  require('./images/15.png'),
  require('./images/16.png'),
  require('./images/17.png'),
  require('./images/18.png'),
  require('./images/19.png'),
  require('./images/20.png'),
  require('./images/21.png'),
  require('./images/22.png'),
  require('./images/23.png'),
  require('./images/24.png'),
  require('./images/25.png'),
  require('./images/26.png'),
  require('./images/27.png'),
  require('./images/28.png'),
  require('./images/29.png'),
  require('./images/30.png'),
  require('./images/31.png'),
  require('./images/32.png'),
  require('./images/33.png'),
  require('./images/34.png'),
  require('./images/35.png'),
  require('./images/36.png')
];

function get_card_img(value, suit) {
  if (value === 0 || suit === 0) {
    return back;
  }

  const base = (14 - value) * 4;

  // The protocol uses Pass-C-D-S-H, the cards use C-S-H-D.
  const suit_offset = [0, 3, 1, 2][suit - 1];
  const card_num = base + suit_offset;
  return cards[card_num];
}

const Card = ({value, suit, direction}) => (
  // TODO: set alts to the value??
  <img src={get_card_img(value, suit)} className={"Card-" + direction} alt="card"/>
);

Card.propTypes = {
  value: PropTypes.number,
  suit: PropTypes.number,
  direction: PropTypes.string
};

Card.defaultProps = {
  value: 0,
  suit: 0
};

export default Card;
