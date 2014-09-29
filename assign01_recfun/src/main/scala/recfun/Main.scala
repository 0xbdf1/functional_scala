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
    if(c < 0 || c > r) 0
    else if(r == 0) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(chars: List[Char], openCount: Int): Int = {
      if(chars.isEmpty) openCount
      else if(chars.head == '(') helper(chars.tail, openCount+1)
      else if(chars.head == ')') {
        if(openCount > 0) helper(chars.tail, openCount-1)
        else -1
      }
      else helper(chars.tail, openCount)

    }
    def openCount:Int = helper(chars, 0)
    openCount == 0
  }
  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    def sortedCoins = coins.sortWith(_ > _)
    def helper(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty) 0
      else if(money - coins.head == 0) 1 + helper(money, coins.tail)
      else if (coins.head > money) helper(money, coins.tail)
      else helper(money-coins.head, coins) + helper(money, coins.tail)
    }
    helper(money, sortedCoins)
  }
}
