import funsets.FunSets._
object T {
  type Foo = Int => Boolean
  val b = 2
  def singletonFoo(elem: Int): Foo = n => n == elem

  def exists2(s: Foo, p: Int => Boolean): Boolean = {
    def iter2(a: Int): Boolean = {
      if (a < -b) false
      else if (s(a) && p(a)) true
      else iter2(a - 1)
    }
    iter2(b)
  }
  def map2(s: Foo, f: Int => Int): Foo = n => {
    print("exists2: n-> " + n + "=" + exists2(s, z => {
      f(z) == n
    }))
    exists2(s, z => {
      println(", z=" + z); f(z) == n
    })
  }

  def toString(s: Foo): String = {
    val xs = for (i <- -b to b if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }
  /**
   * Prints the contents of a set on the console.
   */
  def printFoo(s: Foo) {
    println(toString(s))
  }
}
val s1 = singletonSet(2)
val s2 = singletonSet(4)
val s3 = singletonSet(6)
val s4 = union(union(s1, s2), s3)
val s5 = union(s1, s2)
contains(s1, 2)
contains(s3, 4)
printSet(s3)

printSet(diff(s4, s5))

printSet(filter(s4, x => x > 3))

exists(s4, x => x > 8)
forall(s4, x => x > 8)
forall(s4, x => x < 8)
T.printFoo(T.map2(T.singletonFoo(1), x => x +1))









































































