val t = List(3,6,8,3,5)
val v = List(34,23,45)
(t foldLeft 0) (_ + _)
t.foldLeft(1)(_*_)
t.reduceLeft(_ + _)
t.reduceLeft(_ * _)
t.reduceRight((x,y) => { println("["+ x + "]+[" + y + "]")
  x + y })



def concat[T](xs: List[T], ys: List[T]):List[T] = {
  (xs foldRight ys) (_ :: _ )
}

t.foldRight(v)(_ :: _)

concat(t, v)




