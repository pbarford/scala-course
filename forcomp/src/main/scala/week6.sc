
val xs = Array(1,2,3,5)
val s = "Hello World"

s filter( c => c.isUpper)

s exists(c => c.isUpper)
s forall(c => c.isUpper)

val pairs = List(1,2,3,4,5,6).zip(s)
pairs.unzip

s flatMap(c=> List(',', c))

(1 to 10) flatMap(x => xs.map(y => (x,y)))


def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map { case (x,y) => x * y }.sum

val v1 = Vector(2d,3d,4d,5d)
val v2 = Vector(3d,6d,7d,9d)

scalarProduct(v1, v2)

9 % 9
(2 to 9-1).map(x=> (9,x))

(2 to 9-1).map(x=> (9,x)).forall { case(x,y) => x % y != 0 }

def isPrime(n:Int):Boolean = {
  //(2 to n-1).map(x=> (n,x)).forall { case(x,y) => x % y != 0 }
  (2 until n).forall { d  => n % d != 0 }
}
isPrime(7)