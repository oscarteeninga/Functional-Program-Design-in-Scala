def repeat(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) repeat(command)(condition)
}

var x = 1
repeat {
  println(10);
  x = x - 1
} (condition=false)


//def xor(a: Wire, b: Wire, output: Wire): Unit = {
//  val d, e, f, g = new Wire
//  inverter(a, d)
//  inverter(b, e)
//  and(a, e, f)
//  and(b, d, g)
//  or(f, g, output)
//}

//d == ~a
//e == ~b
//f == a i e == a i ~b
//g == b i d == b i ~a
//c == f lub g == a i ~b lub b i ~a == a != b



