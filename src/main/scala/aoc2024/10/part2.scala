package aoc2024.`10`

import scala.collection.mutable.ArrayBuffer

object Day10Part2 {
  implicit val pointOrdering: Ordering[Point] = Ordering.by(p => (p.y, p.x))

  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/10/"
    val lines = io.Source.fromFile(prefix + "data.txt").getLines.toList
    val mapSize = lines.length
    lines.foreach(println(_))
    println("")
    val trailMap: Map[Point, (Int, Point)] = lines.zipWithIndex.flatMap(line => line._1.split("").map(char =>
      if (char == ".") {
        "-1"
      } else {
        char
      }
    ).zipWithIndex.map(char => { 
      (Point(char._2, line._2) -> (char._1.toInt, Point(char._2, line._2)))
    }).toList).toMap
    
    val trailheads: List[Point] = trailMap.filter { case (_ -> valueLocTuple) =>
      valueLocTuple._1 == 0
    }.map { case (loc -> _) => loc }.toList.sorted

    println(trailheads.map { trailhead =>
      var goals = ArrayBuffer.empty[List[Point]]
      bfs(trailhead, trailMap, 0, mapSize, goals, List())
      // goals.foreach(println(_))
      // println(goals.toSet.size)
      goals.length
    }.sum)
  }

  def bfs(
    from: Point,
    trailMap: Map[Point, (Int, Point)],
    level: Int,
    mapSize: Int,
    goals: ArrayBuffer[List[Point]],
    route: List[Point],
  ): Unit = {
    if (trailMap(from)._1 != level) {
      return
    } else if (level == 9) {
      goals.addOne(route)
    }
    val nextLevel = level + 1
    go(North, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals, route :+ from)
    }
    go(East, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals, route :+ from)
    }
    go(South, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals, route :+ from)
    }
    go(West, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals, route :+ from)
    }
  }

  def go(
    dir: Direction,
    loc: Point,
    trailMap: Map[Point, (Int, Point)],
    mapSize: Int,
  ): Option[(Int, Point)] = {
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
}
