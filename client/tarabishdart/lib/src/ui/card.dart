part of ui;


class CardUI extends Object with Drawable, Clickable {
  int x;
  int y;
  int width = 72;
  int height = 96;
  Card _card;

  CardUI(this.x, this.y, this._card);

  clicked(int x, int y, TarabishSocket socket) {
    socket.play_card(_card);
  }

  void draw(CanvasRenderingContext2D context) {
    context.drawImage(_getCardImage(_card), x, y);
  }

  ImageElement _getCardImage(Card card) {
    var num = (14 - card.value) * 4;
    // Order of suits is different from our suites
    // C - S - H - D
    switch (card.suit) {
      case CLUBS:
        num += 0;
        break;
      case SPADES:
        num += 1;
        break;
      case HEARTS:
        num += 2;
        break;
      case DIAMONDS:
        num += 3;
        break;
    }
    return cardImages[num];
  }
}