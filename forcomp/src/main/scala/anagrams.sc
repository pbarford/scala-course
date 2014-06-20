"test".map(x => (x,1)).groupBy{case(c,_) => c}.mapValues(_.size).toList.sortBy{case (c,_) => c}

def occurances(w: String):List[(Char, Int)] =
  w.map(x => (x,1)).groupBy{case(c,_) => c}.mapValues(_.size).toList.sortBy{case (c,_) => c}

val st = List("eat", "ate", "test", "add", "too")
st.map(t => occurances(t))
st.map(t => occurances(t)).flatten
st.map(t => occurances(t)).flatten.
  groupBy{case(c,_) => c}.
  map(ts => ts._2.reduceRight((x,y) => (x._1, x._2 + y._2))).
  toList.sortBy{case (c,_) => c}


class Dict(val items: Map[List[(Char, Int)], Seq[String]]) {

  def this(bindings: (List[(Char, Int)], Seq[String])*) = this(bindings.toMap withDefaultValue Seq())

  def + (other: Dict) = new Dict(items ++ (other.items map adjust))
  def adjust(item: (List[(Char, Int)], Seq[String])):
        (List[(Char, Int)], Seq[String]) = {
    val  (key, words) = item
    items get key match {
      case Some(value1) => key -> (words ++ value1.seq)
      case None => key -> words
    }
  }

  override def toString =
    (for ((key, item) <- items.toList) yield key+":" + item) mkString " + "
}

//val m = Map(Option)
st.groupBy(word => occurances(word))

//List(('t', 1), ('t', 2)).reduceRight((x,y) => (x._1, x._2 + y._2))
//val t = List(3,6,8,3,5)
//t.reduceRight((x,y) => { println("["+ x + "]+[" + y + "]")
//  x + y })




