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
    print(balance(":())(".toList))
  }

  def factorial(c: Int): Int = if (c == 0) 1 else c * factorial(c-1);
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = factorial(r) / (factorial(c) * factorial(r-c));

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

    if (!chars.isEmpty) findBalance(chars, 0, 0) else true;
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
