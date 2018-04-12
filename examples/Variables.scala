object Variables extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def create(a : Int) : List = {
    a match {
      case 1 => Nil()
      case _ => Cons(1, Nil())
    }
  }

  def pow(b: Int, e: Int): Int = {
    if (e == 0) { 1 }
    else {
      if (e % 2 == 0) {
        val rec: Int = pow(b, e/2);
        rec * rec
      } else {
        b * pow(b, e - 1)
      }
    }
  }

  val x : Int = 4;
  var y : Int = 3;
  var z : Int = pow(2, 3);
  Std.printInt(z);
  z = x;
  z match {
    case 4 => z = 3
    case _ => z = 0
  };

  Std.printInt(z)
}
