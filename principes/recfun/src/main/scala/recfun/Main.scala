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
    * Pascal suite recursive way
    */
  def pascal(c: Int, r: Int): Int = {
    if(c==0 || c==r) 1
    else (pascal(c-1,r-1)+pascal(c,r-1))
  }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

    def iterateChars(balance:Tuple2[Int,Int],chars:List[Char]): Boolean=
      {
      if(chars.isEmpty) {
        if(balance._1==balance._2) true
        else false
      }
      else {
        chars.head match {
          case '(' => iterateChars((balance._1+1,balance._2), chars.tail)
          case ')' => if(balance._2+1>balance._1) false else{
            iterateChars((balance._1,balance._2+1), chars.tail)
          }
          case _ => iterateChars(balance, chars.tail)
        }
      }
      }
    if(chars.isEmpty) true
    else iterateChars((0,0),chars)
}
  
  /**
   * Exercise 3
   */
def countChange(money: Int, coins: List[Int]): Int = {
  if(money == 0)
    1
  else if(money > 0 && !coins.isEmpty)
    countChange(money - coins.head, coins) + countChange(money, coins.tail)
  else
    0
}
}
