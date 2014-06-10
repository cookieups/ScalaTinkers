package recursion

object printPascals {
    
     
  	def pascalsTriangle(row:Int, max:Int, i:Int):Int =
      {
        
      	if(row == max) return 0 
          if(row>0)
          System.out.println()
      		for(i <- 0 to row)
          		System.out.print(nChooseR(row, i)+" ")
        	pascalsTriangle(row+1, max, 1)
        
    }
  
  	def fact(x:Int, acc:Int):Int =
    {
           if(x == 0) return 1
           if(x ==1) return acc
           fact(x-1, acc*x)
    }
  	def nChooseR(n:Int, r:Int):Int =
      {
      	if(n==r) return 1
        if(r==0) return 1
        return fact(n,1)/(fact(r,1)*fact(n-r,1))  
       }
  	def reNchooseR(n:Int, r:Int):Int=
  	{
  	  if(n==r) 1
  	  else
      if(r==0) 1
      else
        reNchooseR(n-1, r)+reNchooseR(n-1,r-1)
  	}
      
  	

    def main(args: Array[String]) {
         /** This will handle the input and output**/
        System.out.println(""+reNchooseR(80,6))

    }
}
