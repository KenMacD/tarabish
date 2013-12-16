part of ui;

class TrumpSelector extends Object with Drawable, Clickable {
  int x = 412;
  int y = 284;
  int width = 200;
  int height = 260;

  ImageElement suitsImage;

  TrumpSelector() {
    suitsImage = new ImageElement(src: "images/suits.png");

  }

  void draw(CanvasRenderingContext2D context) {
    //context.fillText("Please select Trump:", 412, 174);
    context.drawImage(suitsImage, 412, 284);
    var oldFont = context.font;
    context.font = "normal 60px Sans-Serif";
    context.textAlign = 'center';
    context.fillText("PASS", 512, 534);
    context.font = oldFont;
  }

  void clicked(int x, int y, TarabishSocket socket) {
    assert(this.contains(x, y));

    if (x >= this.x && x < this.x + this.width
        && y >= this.y && y < (484 + 60)) {
      // S H
      // D C
      if (y < 384) {
        if (x < 512) socket.callTrump(SPADES);
        else socket.callTrump(HEARTS);
      } else if (y < 484) {
        if (x < 512) socket.callTrump(DIAMONDS);
        else socket.callTrump(CLUBS);
      } else {
        socket.callTrump(PASS);
      }
    }
  }
}