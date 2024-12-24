package aoc2024.util

trait Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

case class Point(x: Int, y: Int)

object twoDmap {
  def go[T](
    dir: Direction,
    loc: Point,
    trailMap: Map[Point, (T, Point)],
    mapSize: Int,
  ): Option[(T, Point)] = {
    dir match {
      case North => {
        if (loc.y == 0) {
          None
        } else {
          Some(trailMap(Point(loc.x, loc.y - 1)))
        }
      }
      case East => {
        if (loc.x == mapSize - 1) {
          None
        } else {
          Some(trailMap(Point(loc.x + 1, loc.y)))
        }
      }
      case South => {
        if (loc.y == mapSize - 1) {
          None
        } else {
          Some(trailMap(Point(loc.x, loc.y + 1)))
        }
      }
      case West => {
        if (loc.x == 0) {
          None
        } else {
          Some(trailMap(Point(loc.x - 1, loc.y)))
        }
      }
    }
  }

  def padded[T](theMap: List[List[T]], padWith: T): List[List[T]] = {
    val padRow = Range(0, theMap(0).length).toList.map(item => padWith)
    ((padRow :: theMap) :+ padRow).map(row => padWith :: (row :+ padWith))
  }

  def listWithPoints[T](theMap: List[List[T]]): List[List[(T, Point)]] = {
    theMap.zipWithIndex.map(row => 
      row._1.zipWithIndex.map(item =>
        (item._1, Point(item._2, row._2))
      )
    )
  }

  def mapWithPoints[T](theMap: List[List[T]]): Map[Point, (T, Point)] = {
    theMap.zipWithIndex.flatMap(row => 
      row._1.zipWithIndex.map(item =>
        (Point(item._2, row._2) -> (item._1, Point(item._2, row._2)))
      )
    ).toMap
  }
}