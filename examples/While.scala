object While extends App {
  var x: Int = 3;
  var y: Int = 0;
  while(x < 10){
		val j: Int = 1;
		y += 1;
		x += j
  };
  Std.printInt(y);
  Std.printInt(x)
}
