part of canvas;

class TarabishCanvas {
  final CanvasElement _canvas;

  TarabishCanvas(this._canvas);

  start() {
    _canvas.context2D.fillText("TESTING", 100, 100);
    // window.requestAnimationFrame();

  }
}