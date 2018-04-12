object Nested extends App {
  def solve(n : Int) : Int = {
    if (n < 1) {
      error("can't solve Hanoi for less than 1 plate")
    } else {
      0
    }
  }

  Std.printString("Hanoi for 4 plates: " ++ Std.intToString(solve(4)))
}