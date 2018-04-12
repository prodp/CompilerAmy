object MatchPass extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def length(l: List): Int = { l match {
    case Nil() => 0
    case Cons(h, t) => 1 + length(t)
    case Cons(h, Cons(i, Nil())) => 2
  }}

  val i :Int = 1;
  val j :Int = 2;
  val x : List = Cons(i, Cons(i, Nil()));
  val y : List = Cons(i, Nil());
  val z : Int = i match {
    case k => if(k == 1) {2} else {0}
    case 2 => 3
  };

  val zz: Int = x match {
    case Cons(1, Cons(2 ,Nil())) => 5
    case _ => 0
  };

  x

}
