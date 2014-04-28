def pascalRow(r:Int): List[Int] = {
  var line = List[Int](1)
  def inner(col: Int, prevCol: Int, prevRes: Int) : List[Int] = {
    val res = (prevRes * (r-prevCol)) / col
    println(prevRes + " * (" + r + " - " + col+")) / (" + col + " + 1) = " + res)
    line = line ::: List[Int](res)
    if(col==r) line
    else inner(col+1, col, res)
  }
  if(r==0) line else inner(1, 0, 1)
  line
}

def pascal(c: Int, r: Int): Int = {
  def inner(col: Int, prevCol: Int, prevRes: Int) : Int = {
    val res = (prevRes * (r-prevCol)) / col
    println(prevRes + " * (" + r + " - " + col+")) / (" + col + " + 1) = " + res)
    if(col==c) {
      println("res:" + res)
      res
    }
    else inner(col+1, col, res)
  }
  if(c<=0 || c>=r) 1 else inner(1, 0, 1)
}
def countChange(money: Int, coins: List[Int]): Int = {
  def change(amount: Int, coin: Int): Int = {
    if(amount == 0) return 1
    if(amount < 0) return 0
    if(coin < 0) return 0
    change(amount, (coin-1)) + change((amount - coins(coin)), coin)
  }
  change(money, coins.length -1)
}

def balance(chars: List[Char]): Boolean = {

  def parenthesesCount(c: Char, count: Int) : Int = {
    c match {
      case '(' => count + 1
      case ')' => count - 1
      case _ => count
    }
  }

  def inner(c: Char, cl: List[Char], count: Int=0, isParenthesesOpen:Boolean = false) : Boolean = {
    if (cl.isEmpty) {
      parenthesesCount(c, count) == 0
    }
    else {
      c match {
        case '(' => inner(cl.head, cl.tail, count + 1, true)
        case ')' => if (!isParenthesesOpen) false else inner(cl.head, cl.tail, count,  (count - 1) != 0)
        case _ => inner(cl.head, cl.tail, count, isParenthesesOpen)
      }
    }
  }
  inner(chars.head, chars.tail)
}

println(pascal(4, 5))






println(pascalRow(5))






println(countChange(4, List(1,2)))











