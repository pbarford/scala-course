import patmat._
import patmat.Huffman._
import patmat.Huffman.Leaf
import scala.Tuple2

val numbers1 = List(5, 4, 8, 6, 2)
numbers1.fold(0) { (z, i) =>
  z + i
}
val numbers = List(1,2,3,4,5);
numbers.zip(numbers.tail).flatMap {
  value => {
    List(value._1+value._2);
  }
};
def reduceByKey(collection: Traversable[Tuple2[Char, Int]]) = {
  collection.groupBy(_._1).map {
    case (group: Char, traversable) => traversable.reduce{(a,b) => (a._1, a._2 + b._2)}
  }
}
val chars = List('a', 'b','a','b','b', 'c')
Huffman.times(chars)
val parts = chars.partition((c: Char) => c == chars.head)
parts._1
parts._2
reduceByKey(chars.map(x => (x,1)))

chars.map(x => (x,1)).groupBy(x => x._1).mapValues(v => v.size).toList


def insert(x:Int, xs: List[Int]) : List[Int] = xs match {
  case List() => List(x)
  case y :: ys => {
    if (x < y) x :: xs else y :: insert(x, ys)
  }
}

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

isort(List(9,3,2))

val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
until(singleton, combine)(leaflist)

createCodeTree(List('v', 'b', 'a', 'c', 'a', 't'))