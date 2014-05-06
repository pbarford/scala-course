object Exercise {

  def main(args: Array[String]) {
    println(sum(x => x * x, 1, 5))
    println(sum(x => x + x, 1, 5))

    println(sum2(x => x + x) (1, 5))
    println(sum3(x => x + x) (1, 5))

    val addSum = (sum2(x => x + x))

    println(addSum(1,5))

    println(product(x => x * x)(3,4))
    println(factorial(5))

    println("1: " + product2(x => { println("1.1: "+ x + " * " + x) ; x * x})(3,4))
    println(factorial2(5))

    println(mapReduce(x => { println("x="+x); x}, (x,y) => {println("(x):"+ x + "* (y):" + y); x * y}, 1)(3,6))

    println(mapReduce(x => { println("x="+x); x}, (x,y) => {println("(x):"+ x + "+ (y):" + y); x + y}, 0)(3,6))

    println(mapReduce2((x,y) => {println("(x):"+ x + "* (y):" + y); x * y}, 1)(3,6))
    println(mapReduce2((x,y) => {println("(x):"+ x + "* (y):" + y); x + y}, 0)(3,4))
    println(mapReduce2((x,y) => {println("(x):"+ x + "* (y):" + y); x * y}, 1)(3,4))

    println("2: " + product2(x => { println("2.1: "+ x + " + " + x) ; x + x})(3,4))
    println(product3(3,4))

  }

  def sum(f: Int => Int, x :Int, y: Int) = {
    def loop(a: Int, acc: Int) :Int = {
      if(a>y) acc
      else loop(a+1, f(a) + acc)
    }
    loop(x, 0)
  }

  def sum2(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int) :Int = {
      if(a>b) 0
      else f(a) + sumF(a+1, b)
    }
    sumF
  }

  def sum3(f: Int => Int)(a: Int, b: Int) :Int =
    if(a>b) 0 else f(a) + sum3(f)(a+1, b)

  def product(f: Int => Int)(a:Int, b: Int): Int =
    if(a > b) 1 else f(a) * product(f)(a+1, b)

  def factorial(n: Int) = product(x => x)(1, n)

  def mapReduce2(combine: (Int, Int) => Int, baseUnit: Int)(a: Int, b:Int):Int =
    if(a>b) baseUnit
    else combine(a, mapReduce2(combine, baseUnit)(a+1, b))

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, baseUnit: Int)(a: Int, b:Int):Int =
  if(a>b) baseUnit
  else combine(f(a), mapReduce(f, combine, baseUnit)(a+1, b))

  def product2(f: Int => Int)(a:Int, b: Int): Int = mapReduce(f, (x,y) => x * y, 1)(a,b)

  def factorial2(n: Int) = product2(x => x)(1, n)

  def product3(a:Int, b: Int): Int = mapReduce2((x,y) => x + y, 0)(a,b)
}
