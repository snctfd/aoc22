import scala.io.Source

extension [T](xss: Vector[Vector[T]])
    def getCol(col: Int) = xss.map(_.apply(col))
    def getRow(row: Int) = xss(row)

object TreeHouse:
    def readGrid(inputFile: String): Vector[Vector[Int]] =
        val lines = Source.fromFile(inputFile).getLines()
        lines.map(_.split("").map(_.toInt).toVector).toVector // map each line into a Vector[Int]

    def isVisible(grid: Vector[Vector[Int]], rowIndex: Int, colIndex: Int): Boolean =
        val elem = grid(rowIndex)(colIndex)
        val col = grid.getCol(colIndex)
        val row = grid.getRow(rowIndex)
    
        val visLeft = row.take(colIndex).forall(_ < elem)
        val visRight = row.drop(colIndex + 1).forall(_ < elem)
        val visUp = col.take(rowIndex).forall(_ < elem)
        val visDown = col.drop(rowIndex + 1).forall(_ < elem)

        visLeft || visRight || visDown || visUp

    def scenicScore(grid: Vector[Vector[Int]], rowIndex: Int, colIndex: Int): Int =
        val elem = grid(rowIndex)(colIndex)
        val col = grid.getCol(colIndex)
        val row = grid.getRow(rowIndex)

        val distLeft = row.take(colIndex).reverse.indexWhere(_ >= elem) 
        val distRight = row.drop(colIndex + 1).indexWhere(_ >= elem) 
        val distUp = col.take(rowIndex).reverse.indexWhere(_ >= elem)
        val distDown = col.drop(rowIndex + 1).indexWhere(_ >= elem) 
        
        val scoreLeft = if distLeft >= 0 then distLeft + 1 else colIndex
        val scoreRight = if distRight >= 0 then distRight + 1 else row.indices.last - colIndex
        val scoreUp = if distUp >= 0 then distUp + 1 else rowIndex
        val scoreDown = if distDown >= 0 then distDown + 1 else col.indices.last - rowIndex

        scoreLeft * scoreRight * scoreUp * scoreDown


    @main
    def run(): Unit =
        val grid = readGrid("input.txt")
        val indices = grid.indices.flatMap(i => grid(i).indices.map(j => (i,j)))
        val visibleCount = indices.count(isVisible(grid, _, _))
        val maxScore = indices.map(scenicScore(grid, _, _)).max
        println(s"visible trees: $visibleCount")
        println(s"max score: $maxScore")       
        