object GameOfLife extends App {
  case class Cell(x: Int, y: Int)
  case class Rel(alive: Boolean, neighbors: Int)

  def cellTick(rel: Rel) = rel.neighbors match {
    case 2 => rel.alive
    case 3 => true
    case _ => false
  }

  def explode(coords: List[Cell]) = for {
    cell   <- coords
    deltax <- List(-1, 0, 1)
    deltay <- List(-1, 0, 1)
    center = (deltax == 0 && deltay == 0)
    neighbors = if (!center) 1 else 0
  } yield Map(Cell(cell.x + deltax, cell.y + deltay) -> Rel(center, neighbors))

  def cellMerge(a: Rel, b: Rel) =
    Rel((a.alive || b.alive), a.neighbors + b.neighbors)

  def worldTick(cells: List[Cell]) =
    explode(cells).
      foldLeft(Map[Cell, Rel]()) {
        case (sum, item) =>
          val key = item.keys.head
          val a = sum.getOrElse(key, Rel(false, 0))
          val b = item(key)
          sum ++ Map(key -> cellMerge(a, b))
      }.filter { case (cell, rel) => cellTick(rel) }.
      keys
}

// 4 Whitespace
// 3 Closing }
// 25 loc