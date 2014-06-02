def msort3[T](l: List[T])(lt: (T,T) => Boolean): List[T] = {
  val n = l.length/2
  if(n==0) l
  else {
    def merge3(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if(lt(x,y)) x :: merge3(xs1, ys)
          else y :: merge3(xs, ys1)
      }
    }
    val (p1, p2) = l splitAt n
    merge3(msort3(p1)(lt), msort3(p2)(lt))
  }
}

val t = List(5,3,8,34,2,78,12)

msort3(t)((x: Int, y: Int) => x < y)