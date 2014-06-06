"test".map(x => (x,1)).groupBy{case(c,_) => c}.mapValues(_.size).toList.sortBy{case (c,_) => c}

def occurances(w: String):List[(Char, Int)] =
  w.map(x => (x,1)).groupBy{case(c,_) => c}.mapValues(_.size).toList.sortBy{case (c,_) => c}

val st = List("test", "add", "too")

st.map(t => occurances(t))
st.map(t => occurances(t)).flatten
st.map(t => occurances(t)).flatten.
  groupBy{case(c,_) => c}.
  map(ts => ts._2.reduceRight((x,y) => (x._1, x._2 + y._2))).
  toList.sortBy{case (c,_) => c}
//List(('t', 1), ('t', 2)).reduceRight((x,y) => (x._1, x._2 + y._2))
//val t = List(3,6,8,3,5)
//t.reduceRight((x,y) => { println("["+ x + "]+[" + y + "]")
//  x + y })




