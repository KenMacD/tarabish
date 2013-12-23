part of ui;

class ScoreBox extends Object with Drawable, Locatable {
  int x;
  int y;
  int width = 100;
  int height = 130;
  
  List<int> usScores = new List();
  List<int> themScores = new List();
  
  ScoreBox(this.x, this.y);
  
  addScores(int us, int them) {
    usScores.add(us);
    themScores.add(them);
  }
  
  void draw(CanvasRenderingContext2D context) {
    
    // Box
    context.beginPath();
    context.rect(x, y, width, height);
    context.fillStyle = 'white';
    context.fill();
    context.lineWidth = 2;
    context.strokeStyle = 'black';
    context.stroke();
    
    // Middle vertical line
    context.beginPath();
    context.moveTo(x + (width / 2) + 1, y);
    context.lineTo(x + (width / 2) + 1, y + height);
    context.stroke();
    
    // Line under Us Them
    context.beginPath();
    context.moveTo(x, y + 20);
    context.lineTo(x + width, y + 20);
    context.stroke();
    
    // "Us" "Them"
    context.beginPath();
    context.font = "12pt Serif";
    context.lineWidth = 1;
    context.textAlign = 'center';
    context.strokeText("Us", x + (width / 4), y + 16);
    context.strokeText("Them", x + (width * 3 / 4), y + 16);
    
    context.beginPath();
    context.font = "8pt Serif";
    context.lineWidth = 1;
    context.textAlign = 'right';
    var liney = y + 30;
    for (var score in usScores) {
      context.strokeText("$score", x + (width / 2) - 15, liney);
      liney += 10;
    }
    liney = y + 30;
    for (var score in themScores) {
      context.strokeText("$score", x + width - 15, liney);
      liney += 10;
    }
  }
  
}