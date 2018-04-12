object While extends App {
  var x: Int = 3;
  val h: Int = 1 + while(x < 10){ //type check error
		val j: Int = 1;
		x = x + j
  };
  while(x < 10){
		val j: Int = 1;
		x = x + j
  }
}
