package recfun
import common._
import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int = 
    if (c==0 || c==r) 1 else pascal(c-1,r-1)+ pascal(c,r-1)

  /**
   * Exercise 2
   */
  
  def balance(chars: List[Char]): Boolean = {
     @tailrec 
     def balanceCount(chars: List[Char], count: Int) : Boolean = {
		//println("b2: " + chars.mkString("") + "," + count )
		if (chars.isEmpty) {
			count == 0
		} else if (chars.head == '(' ) {
			balanceCount(chars.tail,count+1)
		} else if (chars.head == ')') {
			if (count > 0) {
				balanceCount(chars.tail,count-1)
			} else {
			   false
			}
		} else {
			balanceCount(chars.tail, count)
		}
     }        
     balanceCount(chars,0)
  }                                 
    

  /**
   * Exercise 3
   */
  
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
   	else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }           
  
}
