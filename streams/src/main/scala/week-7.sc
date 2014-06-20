def streamRange(lo: Int, hi: Int): Stream[Int] = {
  println(lo + " ")
  if(lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange(1, 10).take(3).toList



def expr = {
  val x = { print ("x"); 1 }
  lazy val y = { print ("y"); 2 }
  def z = { print("z"); 3}

  z + y + x + z + y + x
}

println("\n" + expr)



def isPrime(n:Int):Boolean = {
  //(2 to n-1).map(x=> (n,x)).forall { case(x,y) => x % y != 0 }
  (2 until n).forall { d  => n % d != 0 }
}

(streamRange(1000, 10000) filter isPrime) apply 1














def sieve(s: Stream[Int]): Stream[Int] =
 s.head #:: sieve(s.tail filter (_ % s.head != 0))
val primes = sieve(from(2))
primes.take(200).toList

def from(n: Int): Stream[Int] = n #:: from(n +1)
val nats = from(0)
val m4s = nats map(_ * 4)
(m4s take 30).toList

def isGoodEnough(guess: Double, x:Double) = math.abs((guess * guess - x) / x) < 0.0001
def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #::(guesses map improve)
  guesses
}

sqrtStream(4).take(10).toList

sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList





