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
    print("TheTable Entered View");
    //width = window.innerWidth;
    //height = window.innerHeight;

    _context = getContext("2d");

    _context.fillStyle = "#27462c";
    _context.fillRect(0, 0, window.innerWidth, window.innerHeight);

    _context.fillStyle = "#000000";
    _context.fillText("TESTING", 100, 100);


  }


}