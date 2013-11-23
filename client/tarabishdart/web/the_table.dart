import 'package:polymer/polymer.dart';
import 'dart:html';

@CustomTag('the-table')
class TheTable extends CanvasElement with Polymer, Observable{

  CanvasRenderingContext2D _context;

  TheTable.created() : super.created() {
    print("TheTable Created");
    /* TODO: fill in */
  }

  void enteredView() {
    super.enteredView();
    _context = getContext("2d");
    _context.fillText("TESTING", 100, 100);
  }

}