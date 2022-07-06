package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def verifyInputs: Boolean = {
      if (r < 0 || c < 0 || c > r)
        throw new IllegalArgumentException("Invalid row/column value!!!")
      true
    }

    //    def getPascalNumber(timesMultipliedBy: Int, result: Double): Double = {
    //      // Math.pow(11, r).toDouble
    //      if timesMultipliedBy == 0 then result else getPascalNumber((timesMultipliedBy - 1), 11.0 * result)
    //    }

    //    def getNumberAtSpot: Int = {
    //      println(getPascalNumber(r, 1.0))
    //      getPascalNumber(r, 1.0).formatted("%f").charAt(c).toString.toInt
    //    }

    def getPascalNumber(c: Int, r: Int): Int = {
      if (r < 0 || c < 0 || c > r) then return 0
      if (r == 0 && c == 0) then return 1

      return getPascalNumber(c, r - 1) + getPascalNumber(c - 1, r - 1)
    }

    verifyInputs
    getPascalNumber(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    var startParenthesesCount: Int = 0

    def checkParentheses(chars: List[Char]): Boolean = {
      if startParenthesesCount < 0 then false
      else if chars.isEmpty && startParenthesesCount == 0 then true
      else if chars.isEmpty && startParenthesesCount != 0 then false
      else
        val ch = chars.head
        if ch == '(' then startParenthesesCount = startParenthesesCount + 1;
        else if ch == ')' then
          startParenthesesCount = startParenthesesCount - 1
        checkParentheses(chars.tail)
    }

    return checkParentheses(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) then return 1
    if (money < 0) then return 0
    if (coins.isEmpty) then return 0

    countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}