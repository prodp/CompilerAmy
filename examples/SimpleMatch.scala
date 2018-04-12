object SimpleMatch extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  /*def length(l: List): Int = { l match {
    case Nil() => 0
    case Cons(h, t) => 1 + length(t)
  }}

  0*/
  /*val i : Int = 1 match {
    case one => 40
    case 5 => 30
    case _ => -30
  };*/

  //val l1 : List = Nil();
  //val l2 : List = Cons(30, Cons(31, Nil()));
  val l: List = Cons(5, Nil());
  val l1 : String = l match {
    case Nil() => "Nil"
    case Cons(5, Nil()) => "One Cons!"
    case Cons(5, Cons(5, Nil())) => "Two Cons!!"
  };
  /*val basic : List = Nil();
  val basic1 : Int = basic match {
    case Nil() => 30
  };*/
  /*val l : List = Nil();
  val l1 : Int = l match {
    case Cons(5, Nil()) => 30
    case Nil() => 31
  };*/
  Std.printString(l1)
}
