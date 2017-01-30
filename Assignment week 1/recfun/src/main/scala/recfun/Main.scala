package recfun

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
    * Pascals triangel - algoritm
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    }
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balancer(chars: List[Char], parentheses: Int): Boolean = {
      if (chars.isEmpty) {
        parentheses == 0
      }
      else {
        if (chars.head == '(') {
          balancer(chars.tail, parentheses + 1)
        }
        else if (chars.head == ')') {
          parentheses > 0 && balancer(chars.tail, parentheses - 1)
        }
        else {
          balancer(chars.tail, parentheses)
        }
      }
    }
    balancer(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def changeCounter(money: Int, coins: List[Int], nbrOfChanges: Int): Int = {
      if (money == 0) {
        nbrOfChanges + 1
      }
      else if (money > 0 && coins.nonEmpty) {
        changeCounter(money - coins.head, coins, nbrOfChanges) + changeCounter(money, coins.tail, nbrOfChanges)
      }
      else {
        nbrOfChanges
      }
    }

    changeCounter(money, coins, 0)
  }
}
