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

def oldbalance(chars: List[Char]): Boolean = {

  def inner(i: Int=0, count: Int=0, isParenthesesOpen:Boolean = false) : Boolean = {
    if (i == chars.length) count == 0
    else {
      chars(i) match {
        case '(' => inner(i + 1, count + 1, true)
        case ')' => if (!isParenthesesOpen) false else inner(i + 1, count - 1, (count - 1) != 0)
        case _ => inner(i + 1, count, isParenthesesOpen)
      }
    }
  }
  inner()
}

def balance(chars: List[Char]): Boolean = {

  def loop(c: Char, cl: List[Char], count: Int=0) : Boolean = {

    def updateCount(c: Char, count: Int): Int = {
      c match {
        case '(' => count +1
        case ')' => if(count==0) throw new Exception("invalid close bracket") else count - 1
        case _ => count
      }
    }
    try {
      if (cl.isEmpty) updateCount(c, count) == 0
      else loop(cl.head, cl.tail, updateCount(c, count))
    }
    catch {
      case x: Exception => false
    }
  }
  loop(chars.head, chars.tail)
}
balance("(if (zero? x) max (/ 1 x))".toList)
balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)
balance(":-)".toList)
balance("())(".toList)
balance(")".toList)
def factorial(n: Int) :Int = if (n==0) 1 else n * factorial(n - 1)
def factorial2(n:Int) :Int = {
  def inFact(x: Int, r: Int =1) :Int = {
    //if(x==0) r else inFact(x-1, r + (r * (x-1)))
    if(x==0) r else inFact(x-1, r * x)
  }
  inFact(n)
}
factorial(4)
factorial2(4)
def gcd(a: Int, b:Int):Int = {
  if(b==0) a else gcd(b, a % b)
}
gcd(14,21)
pascal(4, 5)





pascalRow(5)





countChange(4, List(1,2))

