
trait Generator[T] {
  self =>

  def generate: T

  def map[S](f: T => S) : Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate: S = f(self.generate).generate
  }

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  //def choose[T](lo: Int, hi: Int): Generator[T] = for(x <- self.generate) yield (lo + x  (hi - lo))

  //def oneOf[T](xs: T*): Generator[T] = for(idx <- choose)

}



object Test {

  val integers = new Generator[Integer] {
    def generate = {
      val r = new java.util.Random
      r.nextInt()
    }
  }



  val boolean = for (x <- integers ) yield x > 0

  val pairs = for { x <- integers
                    y <- integers} yield (x, y)

  def main(args: Array[String]) {
    println(boolean.generate)
    println(pairs.generate)
    //println(choose(5, 10).generate)
  }
}