object GenericTest extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List
}
