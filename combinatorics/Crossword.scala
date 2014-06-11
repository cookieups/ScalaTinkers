// Solves a 10 X 10 crossword when the words are given
// with + representing dark spaces and - representing open spaces
// e.g. the 4 space part of the puzzle  ++----+  becomes ++spam+
object Crossword{
    
//Defined by the methods below
    val newgrid = buildGrid(0, List[String]()).toArray
    val wordL = wordList()
    val starts = findStarts(newgrid, 0, 0, List[Array[Int]]())
    val it = sortList(wordL).permutations
    val permlist = it.toList

// establishes the fresh crossword to be filled
// called to initialize the search, and also to continue the search when 
// a given permutation of words doesn’t work

    def buildGrid(i:Int, acc:List[String]):List[String]= if(i==10) acc else buildGrid(i+1, acc :+ readLine)

    def wordList():Array[String]= readLine.split(";")
// artifact of testing
    def sortList(words:Array[String]):Array[String]= words.sortBy( x => x)
// artifact of testing
    def printperms(sortedwords:Iterator[Array[String]])= while(sortedwords.hasNext) for(i <- sortedwords.next) System.out.println(i)   
// methods check to see if a given space could be a start of word
    def checkAcross(grid:Array[String], row:Int, col:Int):Boolean=
        col < 9 &&(if(col==0) (grid(row)(0)== '-' && grid(row)(1)== '-' ) else (grid(row)(col) == '-' && (grid(row)(col+1) == '-' && grid(row)(col-1) == '+')))
    def checkDown(grid:Array[String], row:Int, col:Int):Boolean=
        row < 9 &&(if(row==0) (grid(0)(col) == '-' && grid(1)(col) == '-') else (grid(row)(col) == '-' && (grid(row-1)(col) == '+' && grid(row+1)(col) == '-')))
// calls above to compile list of starts        
    def findStarts(grid:Array[String], row:Int, col:Int, acc:List[Array[Int]]):List[Array[Int]]=
        {
            if(row<9)
                if(col<9)
                    if(checkAcross(grid, row, col))
                        if(checkDown(grid,row,col))
                            findStarts(grid, row, col+1, acc ++: List(Array(row,col,0), Array(row,col,1)) )
                        else
                            findStarts(grid, row, col+1, acc ++: List(Array(row,col,0)) )
                    else
                        if(checkDown(grid,row,col))
                            findStarts(grid, row, col+1, acc ++: List(Array(row,col,1)) )
                        else
                            findStarts(grid, row, col+1, acc )
                else
                    if(checkDown(grid,row,col))
                        findStarts(grid, row+1, 0, acc ++: List(Array(row,col,1)) )
                    else
                        findStarts(grid, row+1, 0, acc )
                else
                    if(col<9)
                        if(checkAcross(grid, row, col))
                            findStarts(grid, row, col+1, acc ++: List(Array(row,col,0)) )
                        else
                            findStarts(grid, row, col+1, acc )
                    else
                        return acc
        }
// call to solve, cycles through permutations of words, trying them in order 
// the start points, skipping ahead (total number of words - 
// the current word in the list - 1)! permutations to skip paths that are
// sure not to work.
    def fillPuzzle(grid:Array[String], points:List[Array[Int]],  curr:Array[String], word:Int, max:Int, perms:List[Array[String]]):Array[String]=
        {
            if(max == word)
                return grid
            else
                {
                if(points(word)(2) == 1){
                    if(wordFitDown(grid, points(word)(0), points(word)(1), curr(word))){
                        fillPuzzle(addWordDown(grid, points(word)(0), points(word)(1), curr(word)), points,  curr, word + 1, max, perms)}
                    else
                        {
                        fillPuzzle(newgrid.clone(), points,  skipList(factorial(max - word-1,1),perms).head, 0, max, skipList(factorial(word+1,1),perms))}
                         }
                else
                    {
                    if(wordFitAcross(grid, points(word)(0), points(word)(1), curr(word))){
                        fillPuzzle(addWordAcross(grid, points(word)(0), points(word)(1), curr(word)), points,  curr, word+1, max, perms)}
                    else
                        {
                        fillPuzzle(newgrid.clone(), points, skipList(factorial(max - word-1,1),perms).head, 0, max, skipList(factorial(word+1,1),perms))}
                        }}
        }


    def factorial(n:Int, acc:Int):Int=if(n==1 || n==0) acc else factorial(n-1, acc*n)
        
    def wordFitDown(grid:Array[String], row:Int, col:Int, word:String):Boolean=
        {
            for(i<-0 until word.length)
                {
                     if((row + i) >9) 
                        return false
                     else
                     if(((grid(row+i)(col) != '-') && (grid(row+i)(col) != word(i))) ) 
                        return false
                }
            true
        }
    def wordFitAcross(grid:Array[String], row:Int, col:Int, word:String):Boolean=
        {
                for(i<-0 until word.length)
                {
                     if( (col + i >9) || ((grid(row)(col+i) != '-') && (grid(row)(col+i) != word(i))) )                                 return false
                }
            true
        }
    def addWordDown(grid:Array[String], row:Int, col:Int, word:String):Array[String]=
        {
            
            if(word.length ==0)
                {
                    return grid}
            grid(row) = grid(row).slice(0, col) + word.head + grid(row).slice(col+1, 10)
            addWordDown(grid , row+1, col, word.tail)
        }
    def addWordAcross(grid:Array[String], row:Int, col:Int, word:String):Array[String]=
        {
            grid(row) = grid(row).slice(0, col) + word + grid(row).slice(word.length + col,10)
            return grid
        }
    def printGrid(grid:Array[String])={
        for(i <- grid)
            System.out.println(i)
    }
    def skipList(count:Int, trimmed:List[Array[String]]):List[Array[String]]=
        {
            if(count ==0)
                return trimmed
            return skipList(count-1, trimmed.tail)
        }
    def main(args:Array[String])=
        {
          val finishedgrid = fillPuzzle(newgrid.clone(), starts, permlist.head, 0, starts.length, permlist)
              printGrid(finishedgrid)
         
               
           
               
    }
}