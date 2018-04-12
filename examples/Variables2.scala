object Variables2 extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def doSomething(a: Int) : Unit = {
    var a : Int = 0;
    a += 1;
    Std.printString("No");
    ()
  }

  val z : Unit = doSomething(0);

  var x : Int = 0;
  var y : String = "Hi";
  val w : Int = z match {
    case () => 0
    //case a += 1 => 10
    //case Std.printString("No") => 10
  };

  if(x == 0) {
    val i : Int = 0;
    if(y == "Hi"){
      val j : Int = 5;
      x = i;
      Std.printString("We're inside")
    } else {
      ()
    }
  } else {
    x = 30
  };

  x += 2;
  x += x;
  x *= x;
  y ++= " You";
  /*Std.printInt(x);
  Std.printString(y)*/
  Std.printInt(w)
}
