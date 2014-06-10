// This program finds the sets of integers that whose powers to the n sum to a given number x^n by
// performing a dictionary search of sums, rather than doing the simple recursion to avoid computing unviable paths. See simpleRecursions for the easier way.
// Performed without a local variable
package recursion

object SumOfPowersSets {

//Initialize remainder with the sum to be partitioned. The remainder will be incremented when a piece of the list is removed

//The recursive call returns a list of sets of integers that sum to the given power
  def recurSums(start:Int,remain:Int, powers:List[Int], count:List[Set[Int]], top:List[Int], exp:Int):List[Any]=
    {
       if(powers.isEmpty)
           return count
       if(powers.head == 1)
           {
             remain match {
               case `start` => return count
               case 1 => recurSums(start, remain+top.head, lengthenList(top.head, powers, 2, exp ),  makeSet(1 :: top, Set[Int](), exp) :: count, top.drop(1), exp)
               case _ => recurSums(start, remain+top.head, lengthenList(top.head, powers, 2, exp ), count, top.drop(1), exp) 
                   }
           }
      else
          if(powers.head<remain)
              return recurSums(start, remain-powers.head, trimList(Math.min(powers.head-1, remain - powers.head), powers), count, powers.head :: top, exp)
          else
              return recurSums(start, remain, trimList(powers.head-1, powers),  makeSet(powers.head :: top, Set[Int](), exp) :: count, top, exp )
          
      
      
  }
  def makeSet(top:List[Int], acc:Set[Int], pow:Int):Set[Int]=
      {
          if(top.isEmpty)
              return acc
          makeSet(top.drop(1), acc + (Math.pow(top.head, 1.toDouble/pow).toInt), pow)
  }
      
//Call Math.pow(powers.head, 1/power)+1 to initialize the counter

  def lengthenList(upto:Int, powers:List[Int], counter:Int, power:Int):List[Int]=
      {
          if(powers.isEmpty)
              {
              return List[Int]()
          }
          if(powers.head>=upto)
          {
              return powers.drop(1)
          }
          return lengthenList(upto, intExp(counter, power,1):: powers, counter+1, power)
      
      }
    
  def trimList(max:Int, powers:List[Int]):List[Int]=
    {
        if(powers.isEmpty)
            {
            return powers
        }
    	if(powers.head<=max)
          return powers
        return trimList(max, powers.drop(1))
    }
  //increasing orderQueue
  //Tail Recursion
  //Max is total value, not the integer to be exponentiated 
  //Runs a computes ~ pow*Max computations
  def listPowers(pow:Int, max:Int, curr:Int, acc:List[Int]):List[Int]=
    {
    	if(max >= intExp(curr, pow, 1)){
          return listPowers(pow, max, curr+1, intExp(curr, pow, 1) :: acc)
        }
        return acc
    }
  //tail recursion for intExp
  //O(toThe)
  def intExp(X:Int, toThe:Int, acc:Int):Int=
    {
    	if(toThe==0)
          return acc*1
        return intExp(X, toThe-1, acc*X)
  	}

  def cleanCall(max:Int, pow:Int):Int=
      {
      for(i <-recurSums(max, max, listPowers(pow, max, 1, List[Int]()), List[Set[Int]](), List[Int](), pow)){
          System.out.print("{null")
          for(j<- i.asInstanceOf[Set[Int]]){
                  System.out.print(", " + j)
          }
          System.out.print("}")
          System.out.println()
              }
          return 1
      }
    
    def main(args: Array[String]) {
      cleanCall(readInt(),readInt())
    }
}
