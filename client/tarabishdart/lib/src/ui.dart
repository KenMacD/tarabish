library ui;

import 'dart:html';
import 'package:tarabishdart/src/model.dart';
import 'package:tarabishdart/src/tsocket.dart';

part 'package:tarabishdart/src/ui/trumpselector.dart';
part 'package:tarabishdart/src/ui/card.dart';
part 'package:tarabishdart/src/ui/score.dart';

var cardImages = [];

initUI() {
  var futures = [];
  // TODO: is there an easy way to wait on all these being loaded before allowing starting a game?
  for (var i = 1; i <= 36; i++) {
    var image = new ImageElement(src: "images/$i.png");
    cardImages.add(image);
    futures.add(image.onLoad.first);
  }
  return futures;
}

abstract class Locatable {
  int get x;
  int get y;
  int get width;
  int get height;

  bool contains(int x, int y) {
    if (x < this.x || x > this.x + width)
      return false;
    if (y < this.y || y > this.y + height)
      return false;
    return true;
  }
  
}

// Used for both clickable and double-clickable.
abstract class Clickable {
  void clicked(int x, int y, TarabishSocket socket);
}

abstract class Drawable {
  void draw(CanvasRenderingContext2D _context);
}
