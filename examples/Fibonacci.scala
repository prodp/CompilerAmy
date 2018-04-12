object Fibonacci extends App {
  def fibonacci(n : Int): Int = {
    if(n == 0 || n == 1) {
      n
    } else {
      fibonacci(n - 2) + fibonacci(n - 1)
    }
  }

  Std.printString(Std.intToString(fibonacci(5)))
}