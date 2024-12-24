package aoc2024.`12`

import scala.collection.mutable.HashSet
import aoc2024.util.{North, East, South, West, Point, twoDmap}

object Day12Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/12/"
    val lines = io.Source.fromFile(prefix + "data.txt").getLines.toList

    val theMap = lines.map(line => line.map(char => char.toInt).toList).toList
    val theMapWithPoints: Map[Point, (Int, Point)] = twoDmap.mapWithPoints(theMap)

    // twoDmap.padded(theMap, -1).foreach(println(_))
    // println("")

    var startPoints = theMapWithPoints.keySet
    var cost = 0
    while (startPoints.headOption.isDefined) {
      val newHead = startPoints.head
      val covered = floodRecursive(
          theMapWithPoints(newHead),
          HashSet(),
          theMapWithPoints,
          lines.length
      )
      startPoints = startPoints -- covered
      val fence = perimeter(covered.toSet)
      // println(covered)
      // println(fence)
      println(theMapWithPoints(newHead)._1.toChar + " = " + covered.size + " * " + fence.length)
      // println("")
      cost = cost + covered.size * fence.length
    }
    println("")
    println(cost)
  }

  def perimeter(plot: Set[Point]): List[Point] = {
    plot.toList.flatMap(square => List(
      Point(square.x-1, square.y),
      Point(square.x+1, square.y),
      Point(square.x, square.y-1),
      Point(square.x, square.y+1),
    )).filter(square => !plot.contains(square))
  }

  def floodRecursive(from: (Int, Point), visited: HashSet[Point], theMap: Map[Point, (Int, Point)], mapSize: Int): HashSet[Point] = {
    visited.add(from._2)
    val nextVisits = List(
      twoDmap.go(North, from._2, theMap, mapSize),
      twoDmap.go(East, from._2, theMap, mapSize),
      twoDmap.go(South, from._2, theMap, mapSize),
      twoDmap.go(West, from._2, theMap, mapSize),
    ).flatten
      .filter { case  (plotType, _) => plotType == from._1 }
    // println(nextVisits)
    // val newVisited: Set[Point] = (visited.toSet + from._2) ++ nextVisits.map(_._2)
    visited ++ nextVisits
      .flatMap(next =>
        if (visited.contains(next._2)) {
          visited
        } else {
          floodRecursive(next, visited, theMap, mapSize)
        }
      ).toSet
  }
}

