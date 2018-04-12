object Match extends App {
  def f(i: Int): Int = {
    i match {
      case 1 => 2
      case 2 => 3
    }
  }
  f(1)
}