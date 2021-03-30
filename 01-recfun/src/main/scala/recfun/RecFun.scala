package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def runningCount(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty && count == 0) true
        else if (count < 0 || (chars.isEmpty && count != 0)) false
        else if (chars.head == '(')
          runningCount(chars.tail, count + 1)
        else if (chars.head == ')')
          runningCount(chars.tail, count - 1)
        else runningCount(chars.tail, count)
      }

    runningCount(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0 || (coins.length == 1 && (money % coins.head != 0))) 0
    else if (money == 0 || (coins.length == 1 && (money % coins.head == 0))) 1
    else {
      val sortedCoins = coins.sortWith(_ > _)
      countChange(money - sortedCoins.head, sortedCoins) +
        countChange(money, sortedCoins.tail)
    }
  }
}
