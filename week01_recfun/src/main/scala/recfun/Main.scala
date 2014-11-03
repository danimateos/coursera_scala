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

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkBalance(accumulated: Int, chars: List[Char]): Boolean = {
      if (accumulated < 0 || chars.isEmpty && accumulated != 0) false
      else if (chars.isEmpty && accumulated == 0) true
      else checkBalance(accumulated + parenValue(chars.head), chars.tail)
    }

    def parenValue(char: Char) = {
      if (char == '(') 1
      else if (char == ')') -1
      else 0
    }
    checkBalance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Char]): Int = {
    if (money < 0) return 0
    if (coins.isEmpty) return 0
    if (money == 0) return 1
    countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
