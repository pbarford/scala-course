import funsets.FunSets._


def map2(s: Set, f: Int => Int): Set = n => {
  //print("exists: n-> " + n +"=" +exists(s, z => { f(z) == n}))
  //exists(s, z => { println(", z=" + z); f(z) == n})
  exists(s, z => { f(z) == n})
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
printSet(map2(s4, x => x + 3))








