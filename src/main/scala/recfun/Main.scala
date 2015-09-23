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
  }

  def factorial(c: Int): Int = if (c == 0) 1 else c * factorial(c-1)
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c != 0 && r != c) factorial(r) / (factorial(c) * factorial(r-c)) else 1

  /**
   * Exercise 2
   */


  def balance(chars: List[Char]): Boolean = {
    def findBalance(chars: List[Char], open:Int, close:Int): Boolean = {
      if (open < close) false
      else if (chars.isEmpty) open == close
      else if (chars.head == '(') findBalance(chars.tail, open + 1, close)
      else if (chars.head == ')') findBalance(chars.tail, open, close + 1)
      else findBalance(chars.tail, open, close)
    }

    if (!chars.isEmpty) findBalance(chars, 0, 0) else true
  }

  /**
   * Exercise 3
   */
  //This is a simple to be or not to be problem, a coin can be in a sum or not... -the else part-
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money-coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }
}
