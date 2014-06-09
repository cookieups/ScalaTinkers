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
  def pascal(c: Int, r: Int): Int = 
  {
    if(c == 0 || c == r) 1 else
      pascal(c, r-1) + pascal(c-1, r-1)    	
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
	  def recurBalance(xs: List[Char], parity:Int):Boolean=
	  {
		  if(xs.isEmpty) (parity == 0)
		  else
		    if(parity<0) false else
		      recurBalance(xs.tail, xs.head match {
		        case '(' => parity +1
		        case ')' => parity -1
		        case _ => parity})
	  }
  	  recurBalance(chars, 0)
		      
		    
	  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int ={
    def getNth(n:Int, c:List[Int]):Int = if(n == 1) c.head else getNth(n-1, c.tail)
    def count(m:Int, c:List[Int], max:Int):Int =
    {
      if(m == 0) 1 else if(m<0) 0 else if(max == 0) 0 else count(m, c, max-1) + count(m-getNth(max, c), c, max)
    }
    count(money, coins, coins.length)
  }
}
