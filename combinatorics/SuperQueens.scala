// This program solves the ways to arrange N "Super-Queens" on an
// N X N chessboard. A Super Queen can move as a normal chess queen,
// horizontally, vertically, and diagonally across spaces but can also
// move as a Knight does, in any L shape possible using two spaces in a line
// with one space perpindicular.
// The underlying methodology is that of a change counter algorithm, where one sums
// the arrangements given by a queen placement, and sums the arrangements absent
// that placement.
object SuperQueens {
    // defining a tuple as a Space for readability
    // and a board as an Array of Lists of Spaces
    type Space = (Int,Int)
    type Board = Array[List[Space]]

    def solveNSuperQueens(n: Int): Int = {
        return solveQueens(n ,1,newBoard(n, n, new Board(n)))
}

    def solveQueens(N:Int, row:Int, b:Board):Int=
        {
            if(row == N)
                {
                    if(b(N-1).isEmpty) return 0 else return b(N-1).length
                }
            else
                {
                    //printBoard(b)
                    if(b(row-1).isEmpty) return 0 
                        else 
                    {
                        if(b(row-1).tail.isEmpty) 
                            return solveQueens(N, row+1,  addQueen(b(row-1).head , b.clone(), row)) 
                        else
                        {
                            return solveQueens(N, row+1, addQueen(b(row-1).head, b.clone(), row)) + solveQueens(N, row, removeHead(b, row))
                        }
                    }
                }
        }
    // This is called to remove a point from consideration while moving across a row
    def removeHead(b:Board, row:Int):Board=
    {
        b(row-1) = b(row-1).tail
        return b
    }

    // This builds a new board from the bottom up
    def newBoard(N:Int, row:Int, acc:Board):Board=
        {
        def addRow(N:Int, row:Int, col:Int, acc:Board):Board=
            {
            if(col<1) return acc
                else {
                acc(row-1) =  new Space(row, col) +:acc(row-1)
                return addRow(N, row, col-1, acc)}
            }
            
            if(row<1)
                return acc
            else
                {acc(row-1) = List[Space]()
                    return newBoard(N, row-1, addRow(N, row, N, acc))}
        }


    
    // This method iterates through rows below the addition of the queen, and 
    // filters out spaces that the queen now threatens
    def addQueen(queen:Space, b:Board, row:Int):Board=
        {
            if(row > b.length) return b
                else {
                b(row-1) = b(row-1).filter((s:Space) => checkSpace(queen, s))
                return addQueen(queen, b , row+1)}
                
        }

    // This checks if a space is not threatened by a queen placement

    def checkSpace(queen:Space, p:Space):Boolean = 
    {
        def exp(i:Int):Int = i * i
        def checkSafe(p:(Int,Int)):Boolean= p._1 >0 && p._2 >0 && p._1 + p._2 > 8 && p._1 != p._2
        checkSafe((exp(queen._1 - p._1), exp(queen._2 - p._2)))
    }
    
    // An testing method
    def printBoard(b:Board)={
        for(i <- b){
            for(j <- i)
                print("("+j._1+","+j._2+") ")
            println()
        }
    }
    
    def main(args: Array[String]) {
        println(solveNSuperQueens(readInt))
    }
}
