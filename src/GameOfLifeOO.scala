object GameOfLife {
  import language.implicitConversions
  type RichedCell = Map[Cell, Rel]
 
  case class Board(cells: RichedCell) {
    private def cellTick(rel: Rel) = rel.neighbor match {
      case 2 => rel.alive
      case 3 => true
      case _ => false
    }

    def nextGen: List[Cell] = cells.collect { case (a, b) if cellTick(b) => a }.toList
   
    def ++(other: Board) = {
      val cs = cells.foldLeft(other.cells) {
        case (sum, (cell, rel)) =>
          val sr = sum.getOrElse(cell, Rel(false, 0))
          sum ++ Map(cell -> Rel(rel.alive || sr.alive, rel.neighbor + sr.neighbor))
      }
      Board(cs)
    }
  }

  case class Rel(alive: Boolean, neighbor: Int)
  case class Cell(x: Int, y: Int)
 
  implicit def cellToBoard(c: Cell) = {
    val blocks = for {
      daltax <- List(-1, 0, 1)
      daltay <- List(-1, 0, 1)
      isCenter = (daltay == 0 && daltax == 0)
    } yield Map(Cell(c.x + daltax, c.y + daltay) -> Rel(isCenter, if (isCenter) 0 else 1))
    Board(blocks.foldLeft(Map[Cell, Rel]()) { case (sum, item) => sum ++ item })
  }
 
  def run(cells: List[Cell]) = cells.foldLeft(Board(Map[Cell, Rel]()))(_ ++ _).nextGen
 
  def main(args: Array[String]): Unit = {
    val cells = Cell(1, 2) ++ Cell(1, 0) ++ Cell(1, 1)
    val snd = run(cells.nextGen)
    val third = run(snd)
    val fourth = run(third)
    println(fourth)
  }
}

// 7 Whitespace
// 7 Closing }
// 31 loc