
def last[T](l: List[T]): T = {
  l match {
    case List() => throw new Error("empty list")
    case List(x) => x
    case h :: t => last(t)
  }
}

def init[T](l:List[T]): List[T] = {
  l match {
    case List() => throw new Error("empty list")
    case List(x) => List()
    case h :: t => h :: init(t)
  }
}

def concat[T](xs: List[T], ys: List[T]):List[T] = {
  xs match {
    case List() => ys
    case hs :: ts => hs :: concat(ts, ys)
  }
}

def reverse[T](l: List[T]):List[T] = {
  l match {
    case List() => l
    case h :: t => reverse(t) ++ List(h)
  }
}
def removeAt[T](n: Int, l: List[T]):List[T] = {
  def loop[T](xs: List[T], rs: List[T], pos: Int):List[T] = {
    xs match {
      case List() => rs
      case List(x) => rs ++ List(x)
      case h :: t => if(pos==n) rs ::: t else loop(t, rs ++ List(h), pos+1)
    }
  }
  loop(l, List(), 0)
}

def removeAt2[T](n: Int, l: List[T]):List[T] = (l take n) ::: (l drop n+1)





val x = List(2,4,5,6)
val y = List(7,8,9,2)
last(x)
init(x)
concat(x, y)
reverse(y)
removeAt(2, x)
removeAt2(2, x)
