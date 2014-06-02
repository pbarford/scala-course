def msort(l: List[Int]): List[Int] = {
  val n = l.length/2
  if(n==0) l
  else {
    def merge(xl: List[Int], yl: List[Int]): List[Int] = {
      xl match {
        case Nil => yl
        case xlh :: xlt =>
          yl match  {
            case Nil => xl
            case ylh :: ylt =>
              if(xlh < ylh) xlh :: merge(xlt, yl)
              else ylh :: merge(xl, ylt)
          }
      }
    }
    val (p1, p2) = l splitAt n
    merge(msort(p1), msort(p2))
  }
}

def msort2(l: List[Int]): List[Int] = {
  val n = l.length/2
  if(n==0) l
  else {
    def merge2(xs: List[Int], ys: List[Int]): List[Int] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if(x < y) x :: merge2(xs1, ys)
          else y :: merge2(xs, ys1)
      }
    }
    val (p1, p2) = l splitAt n
    merge2(msort2(p1), msort2(p2))
  }
}

val t = List(5,3,8,34,2,78,12)

msort(t)
msort2(t)