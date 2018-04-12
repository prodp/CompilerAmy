object BindersDuplicate extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def length(l: List): Int = { l match {
    case Nil() => 0
    case Cons(h, t) => 1 + length(t)
    case Cons(h, Cons(i, l)) => l match {
      case Nil() => 2
      case l => 2 + length(l)
    }
  }}

  val i :Int = 1;
  val j :Int = 2;
  val x : List = Cons(i, Cons(i, Nil()));
  val y : List = Cons(i, Nil());

  i
}