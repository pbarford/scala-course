def sqrList(xs: List[Int]):List[Int] = xs match {
  case Nil => xs
  case h :: ts => h * h :: sqrList((ts))
}

def sqrList2(xs: List[Int]):List[Int] = {
  xs.map(x => x * x)
}

val t = List(3,4,5)

sqrList(t)
sqrList2(t)

def pack[T](xs: List[T]): List[List[T]] = {
  xs match {
    case Nil => Nil
    case y :: ys =>
      xs span(v => v == y) match {
        case (l, Nil) => List(l)
        case (Nil, r) => List(r)
        case (l, r) => l :: pack(r)
      }
  }
}

def pack2[T](xs: List[T]): List[List[T]] = {
  xs match {
    case Nil => Nil
    case y :: ys =>
      val (first,rest) = xs span(v => v == y)
      first :: pack2(rest)
  }
}

def encode[T](xs: List[T]):List[(T, Int)] = {
  pack(xs) map(ys => (ys.head, ys.length))
}
val d = List('a', 'a', 'a', 'b', 'c', 'c', 'a')
d span(z=> z == 'a')
pack(d)
pack2(d)

encode(d)