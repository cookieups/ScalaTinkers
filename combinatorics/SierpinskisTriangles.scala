package recursion

object SierpinskisTriangles {
    
  //create iteration array of all underscores.
  //Subdivide into top(underscores), midtop, top right(underscores)
  //bottom left, bottom right. Repeat iteration with iteration count decrement
  //height halved. Size of box 32 by 64. Max of 2^5 for inputs
   def drawTriangles(iter:Int, height:Int):Array[String] =
{
     //base case returns a rectangle filled with a triangle
     if(iter==0)
       return drawTriangle(Array[String](),height, 0)
     //if not base case, subdivide the accumulator array into 3 top rectangles, and two bottoms
     //top left, top right become underscores, top mid, bleft, bright get recursive call
     //horizontally combine tops, and bottoms with hcombineArray, then vertically combine combination with Array.Concat
     return Array.concat(
       //top split
       threecombineArrays
       	(
       //top left
      	  drawUnderBox(Array[String](), height/2, 0),
       //top middle
       	 drawTriangles(iter-1, height/2),
       //top right
          drawUnderBox(Array[String](),height/2,0),
         //Accumulator
         Array[String](),
         0
        )
     ,
       //bottom
       threecombineArrays
       (
         drawTriangles(iter-1, height/2),
         drawUnderSliver(Array[String](),height/2,0),
         drawTriangles(iter-1, height/2),
         Array[String](),
         0
       )
     )
     
}
  
  //creates an array representing a rectangular box containing a filled triangle of height by 2*height +1
  	def drawTriangle(acc:Array[String], height:Int, depth:Int):Array[String] =
      {
            if(depth>height-1)
              {
              	return acc
              }
      		drawTriangle(Array.concat(acc, Array[String]((buildUnderscore("", height-depth-1, 0) + buildOnes("", 1 + 2*depth, 0) + buildUnderscore("", height-depth-1,0)))), height, depth+1)
      }
 
  //builds a square of side length height
  	def drawUnderBox(acc:Array[String], height:Int, count:Int):Array[String]={
      	if(count==height)
          return acc
        return drawUnderBox(Array.concat(acc, Array[String](buildUnderscore("", height, 0))), height, count+1)
    }
//To Draw between bottom two triangles
    def drawUnderSliver(acc:Array[String], height:Int, count:Int):Array[String]={
      	if(count==height)
          return acc
        return drawUnderSliver(Array.concat(acc, Array[String](buildUnderscore("", 1, 0))), height, count+1)
    }
//Creates a line of underscores of length max
  	def buildUnderscore(s:String, max:Int, acc:Int):String = {
		if(max == acc)
			return s
		buildUnderscore(s + "_", max, acc+1)
    }
//Creates a line of ones of length max
  	def buildOnes(s:String, max:Int, acc:Int):String = {
		if(max == acc)
			return s
		buildOnes(s + "1", max, acc+1)
    }
  	//prints an array
  	def printArray(toprint: Array[String]) = {
      for(s <- toprint)
        System.out.println(s)
    }
  	//combines 3 Arrays of same size. DOESNT CHECK ARRAY FOR LENGTH MATCHING.
  	//(array1, array2, array3, accumulatorarray, 0)
  
  def threecombineArrays(aarr:Array[String], barr:Array[String],carr:Array[String], comb:Array[String], depth:Int):Array[String]=
      {
      	if(depth==aarr.length)
          return comb
      threecombineArrays(aarr,barr,carr, Array.concat(comb, Array[String](aarr(depth)+barr(depth)+carr(depth))), depth+1)
        
    }
    def main(args: Array[String]) {
        printArray(drawTriangles(readInt(), 32))
    }
}


