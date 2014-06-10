// This program performs a graham scan to order the points, and then iterates counter clockwise through each, making sure that it takes no right turns. If it takes a right turn, it reverses until it can continue without taking a right turn. This ensures convexity. 
object Solution{
//Computes the sin of the angle between two vectors
//To compare angles between three points for the grahamScan when the cross product = 0
// use the sin function twice using differences between point of interest and current point
// minus the sin of current point - previous point
// if this value is close to zero, it means the third(test) point lies forward on the line
// if this value is large, it means the points are anti parallel
// Notes about the method: Returns between 0 to 4. 3 represents 270 degrees ccw

  def sin(start:List[Int], end:List[Int]):Double=
    {
    	val diffx =  end(0) - start(0)
        val diffy = end(1)-start(1)
        if(diffy > 0){
        if(diffx>0){
          return (diffy)/Math.sqrt(diffx*diffx+diffy*diffy)
        }
    	
    	return 2-diffy/Math.sqrt(diffx*diffx+diffy*diffy)
        }
   		 else{
    	if(diffy<0)
          {
          if(diffx<0){
          return 2 - diffy/Math.sqrt(diffx*diffx+diffy*diffy)
        }
    	return 3 - diffy/Math.sqrt(diffx*diffx+diffy*diffy)
        }
     	else
          {
          if(diffx<0)
          {
            return 2
          }
          else
            return 0
        }
        }
  }
  //Returns a cross product boolean test
  //If the line from test to next forms a left turn from the line from pre to test
  //It returns true
  //It also returns true if the test to next is collinear with pre to test,
  //False for right turns and anti parallel lines.

  def posCross(pre:List[Int], test:List[Int], next:List[Int]):Boolean=
    {
    	var cross = (test(0)-pre(0))*(next(1)-test(1)) - (test(1)-pre(1))*(next(0)-test(0))
        if(cross>0)
        {return true}
        if(cross==0)
          {
          if(Math.abs(sin(List(0,0), List(next(0)-test(0), next(1)-test(1))) - sin(List(0,0), List((test(0)-pre(0)) ,test(1)-pre(1)))) <0.0000000001){return true}}
    	return false
  	}
    //Takes two points and computes their euclidean distance
  def dist(start:List[Int], end:List[Int]):Double=
    {Math.sqrt(Math.pow((end(0)-start(0)),2) + Math.pow((end(1)-start(1)),2))}
    //Performs a graham scan on an ordered list, requires that the accumulator array
    //acc be pre processed to have the lowest point, and the first point of the ordered list
    // be pushed onto the List, and the acclength be initialized with a value of 2
    // The length should be initialized with the value of order that remains following the push
  def grahamScan(order:List[List[Int]], acc:List[List[Int]],length:Int,acclength:Int):List[List[Int]]=
    {
    	if(1==length)
        {return order(0) :: acc}
    	if(posCross(acc(0),order(0),order(1)))
           {return grahamScan(order.drop(1), order(0) :: acc, length-1, acclength+1)}
        else
          if(acclength>2)
            {return grahamScan( acc(0)::order.drop(1) , acc.drop(1), length,acclength-1)}
    	else
          {return grahamScan( order.drop(1) , acc, length-1, acclength)}
  }
  //Adds up the distance between points in sequence of a convex hull
  //Needs to be initialized with the first point of the convex hull as compare
  // and convexHull should have that point dropped from it.
  def sumDistance(acc:Double,  compare:List[Int], convexHull:List[List[Int]], length:Int):Double=
  {
  	if(length==0)
      return acc
    else
      return sumDistance(dist(compare, convexHull(0))+acc, convexHull(0), convexHull.drop(1), length-1)
      }
// Main uses the sortBy algorithm to sort the array by sin of the angle to the lowest point 

  def main(args:Array[String]){
    val num = readInt()
    var s = Array[String]()
    s = readLine().split(" ")
    var templow = List(s(0).toInt, s(1).toInt)
    var toAdd = List[Int](-1,-1)
    var points = List[List[Int]]()
  	  for(i <- 2 to num)
      {
      s = readLine().split(" ")
      toAdd = List(s(0).toInt, s(1).toInt)
      if(toAdd(1)>=templow(1))
       {
	  	points = points ::: List(List(s(0).toInt,s(1).toInt))
        }
      else
        {
        points = points ::: List(templow)
        templow = toAdd
      	}
      }
    
    //sort algorithm
    System.out.println("Lowest x:"+ templow(0) + " y:" + templow(1))
    // Ordering by the sin of the angle from the lowest point
    // to every other point is what’s known as the graham scan
    // sin is considered larger than 1 for values past 90 degrees
    val sorted = points.sortBy(x => sin(templow, x))
    val convexHull = grahamScan(sorted.drop(1) :+ templow, List[List[Int]](sorted(0),templow), num-1, 2)
    System.out.println(""+convexHull.length)
    if(convexHull(0)(0)==templow(0) &&convexHull(0)(1)==templow(1) )
      	System.out.println(""+ sumDistance(0, convexHull(0), convexHull.drop(0), convexHull.length))
      }}