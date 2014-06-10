package recursion

object RecursiveTree{
  //returns a tree of height 2*iter
  // Up to size 5
  def fuseTreeLevels(acc:Array[String],count:Int):Array[String]=
    {
    	if(count == 0)
          return acc
        return fuseTreeLevels(Array.concat(acc, makeTreeLevel(exp(count-1,2), makeTree(exp(6-count,2)),1)), count-1)
  	}
  def makeTreeLevel(iter:Int, acc:Array[String], count:Int):Array[String]=
    {
    //System.out.println("Level combine" + count)
    if(count == iter)
      return acc
    //System.out.println("Make tree of height" + exp(6-iter, 2))
    makeTreeLevel(iter, arrayStitch(acc, makeTree(32/iter), Array[String](), 32/iter, 0 ), count+1)
  }
  
  def makeTree(height:Int):Array[String]=
    {
    return makeFork(makeStem(Array[String](), height/2, 0, stemPiece(underString(height-1,""))), height/2,  0) 
  	}
  //This returns the sum of lateral moves at a given iteration level
  def widthSum(it:Int, acc:Int):Int=
  { 
      if(it==0)
        return acc
      return widthSum(it-1, acc + exp(6-it, 2))
  }
  def exp(pow:Int, topow:Int):Int=
    {
    	if(pow <=0)
          return 1
        return topow*exp(pow-1, topow)
  	}
  def arrayStitch(left:Array[String], right:Array[String], acc:Array[String], iter:Int, index:Int):Array[String]=
   {
    	if(iter==index)
          return acc
   		return arrayStitch(left, right, Array.concat(acc, Array(left(index)+right(index))), iter, index+1)
  }
  def underList(prepped:String, height:Int, acc:Array[String]):Array[String]=
    {
    	if(height ==0)
          return acc
        underList(prepped, height-1, Array.concat(acc, Array(prepped)))
    }
  def underString(length:Int, acc: String):String=
    {
    	if(length == 0)
          return acc
       	return underString(length-1, acc + "_")
  	}
  //call with underString(2^iteration amount - 1,""))
  def stemPiece(left:String):String=
    {
    	return left+"1"+ left+"_"
  	}
  //call with for stem, stemPiece(underString(2^iteration amount - 1,""))
  //height needs to be explicitly calculated
  def makeStem(acc:Array[String], height:Int, count:Int, stem:String):Array[String]=
    {
    	if(height==count)
          return acc
    	makeStem(Array.concat(acc, Array(stem)), height, count+1, stem)
  }
  //add with make stem in the accumulator call, height, iter, 
  //iter = 2^(iteration amount -1) + 1
  def makeFork(acc:Array[String], height:Int, count:Int):Array[String]=
    {
    	if(height == count)
          return acc
       return makeFork(Array.concat(Array(forkPiece("", count, height*2)), acc), height, count+1)
  	}
  //let length be given by the width of 2^(iteration amount-1) + 1
  def forkPiece(acc:String, count:Int, length:Int):String=
    {
    	return underString(length-count-2,"") + 1 + underString(1+2*count,"") + 1 + underString(length-count-1,"")
	}
  def topOff(acc:Array[String], amount:Int, count:Int):Array[String]=
    {
    	if(count==amount)
          return acc
        return topOff(Array.concat(Array[String](underString(64,"")),acc), amount, count+1)
  }
  def finish(side:Array[String], tree:Array[String]):Array[String]=
    {
    	return arrayStitch(arrayStitch(side, tree, Array[String](), 63, 0), side, Array[String](), 63, 0)
  }
	def callNice(iterations:Int):Array[String]=
    {
      	finish(underList(underString(18,""),63, Array[String]()), topOff(fuseTreeLevels(Array[String](),iterations), 63 - widthSum(iterations, 0),0))
    }
  
  def main(arg:Array[String])=
    {
		for(i <- callNice(readInt()))
        //for(i <- makeTreeLevel(4,makeTree(8),1))
          System.out.println(i)
  }
}