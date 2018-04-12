object TestEqualities extends App {

  def bothEquals(x: Int, y: Int, i: Int): Unit = {
    if( (if(x == y) { false} else {true}) ) { Std.printString("Both Equals to "  ++ Std.intToString(i)  ++ " maybe") }
    else { Std.printString("Nope")}
  }

  bothEquals(2, 2, 2);
  bothEquals(3, 2, 3)

}
