package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def inner(col: Int, prevCol: Int, prevRes: Int) : Int = {
      val res = (prevRes * (r-prevCol)) / col
      if(col==c) res
      else inner(col+1, col, res)
    }
    if(c<=0 || c>=r) 1 else inner(1, 0, 1)
  }

  /**
   * Exercise 2
   */
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


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0 || coins.isEmpty) 0
      else countChange((money - coins.head), coins) + countChange(money, coins.tail)
  }
}
