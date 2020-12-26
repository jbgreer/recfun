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
   */
    def pascal(c: Int, r: Int): Int = {
      if (r == 0 || c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def bal(n: Int, chars: List[Char]) : Boolean = {
        if (chars.isEmpty) n == 0
        else if (chars.head == ')') {
          if (n == 0) false
          else bal(n-1, chars.tail)
        } else if (chars.head == '(') bal(n+1, chars.tail)
        else bal(n, chars.tail)
      }
      if (chars.isEmpty) true
      else bal(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
