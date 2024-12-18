package aoc2024.`10`

import scala.collection.mutable.ArrayBuffer

sealed trait Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction

case class Point(x: Int, y: Int)

object Day10Part1 {
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
    }.map { case (loc -> _) => loc }.toList

    println(trailheads.map { trailhead =>
      var goals = ArrayBuffer.empty[Point]
      bfs(trailhead, trailMap, 0, mapSize, goals)
      goals.toSet.size
    }.sum)
  }

  def bfs(
    from: Point,
    trailMap: Map[Point, (Int, Point)],
    level: Int,
    mapSize: Int,
    goals: ArrayBuffer[Point],
  ): Unit = {
    if (trailMap(from)._1 != level) {
      return
    } else if (level == 9) {
      goals.addOne(from)
    }
    val nextLevel = level + 1
    go(North, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals)
    }
    go(East, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals)
    }
    go(South, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals)
    }
    go(West, from, trailMap, mapSize) collect {
      case (_, loc) => bfs(loc, trailMap, nextLevel, mapSize, goals)
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
