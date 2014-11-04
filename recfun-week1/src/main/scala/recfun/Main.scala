package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 until 10)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if ((c == 0) || (c == r) || (r == 0)) 1
    else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], counter: Int): Int = {
      if (chars.isEmpty) counter
      else {
        chars.head match {
          case '(' => if (counter < 0) counter else loop(chars.tail, counter + 1)
          case ')' => if (counter < 0) counter else loop(chars.tail, counter - 1)
          case _ => loop(chars.tail, counter)
        }
      }
    }

    loop(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else {
      (money, coins) match {
        case (0, List(_)) => 1
        case (v, Nil) => 0
        case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
      }
    }
  }
}
