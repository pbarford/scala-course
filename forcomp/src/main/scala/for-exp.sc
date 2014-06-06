
case class Person(name:String, age: Int)

val persons = List(Person("alexandra", 8), Person("paul", 21), Person("anita", 22))
for ( p <- persons if p.age > 20) yield p.name

val n = 7

def isPrime(n:Int):Boolean = {
  //(2 to n-1).map(x=> (n,x)).forall { case(x,y) => x % y != 0 }
  (2 until n).forall { d  => n % d != 0 }
}

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i+j)
} yield (i,j)