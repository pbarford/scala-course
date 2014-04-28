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

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0 || coins.isEmpty) 0
      else countChange((money - coins.head), coins) + countChange(money, coins.tail)
  }
}
