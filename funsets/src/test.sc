import funsets.FunSets._
val b = 2
def exists2(s: Set, p: Int => Boolean): Boolean = {
  def iter2(a: Int): Boolean = {
    if(a < -b) false
    else if(s(a) && p(a)) true
    else iter2(a-1)
  }
  iter2(b)
}
def map2(s: Set, f: Int => Int): Set = n => {
  print("exists2: n-> " + n +"=" +exists2(s, z => { f(z) == n}))
  exists2(s, z => { println(", z=" + z); f(z) == n})
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
printSet(map2(singletonSet(1), x => x + 1))











































