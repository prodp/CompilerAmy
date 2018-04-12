object TestCodeGen extends App {
  def sum(a: Int, b: Int): Int =  {
    a + b
  }

  val a : Int = 1;
  val b : Int = 2;
  val c : Int = sum(a, b);

  //val d : Int = if(a == 1 && b == 0 && c == 3) {30} else {-30};
  //val e : Int = if(a == 0 || b == 2) {0} else {-1};
  val f : Int = if(a == 1 || b == 0) {30} else {-30};

  Std.printInt(sum(a, c))

}
