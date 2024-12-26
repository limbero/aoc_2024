package aoc2024.`12`

import scala.collection.mutable.HashSet
import aoc2024.util.{North, East, South, West, Point, twoDmap}
import scala.collection.mutable.ArrayBuffer

object Day12Part2 {
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
      val numSides = countSides(fence)
      // println(covered)
      // println(fence)
      // if (numSides % 2 == 1) {
      //   println(theMapWithPoints(newHead)._1.toChar + " = " + covered.size + " * " + numSides)
      // }
      println(theMapWithPoints(newHead)._1.toChar + " = " + covered.size + " * " + numSides)
      // println("")
      cost += covered.size * numSides
      // println(cost)
    }
    println("")
    println(cost)
  }

  def countSides(perimeter: List[(Point, Char)]): Int = {
    var numSides = 0
    var headTuple :: tail = perimeter
    var (head, headChar) = headTuple
    var break = false
    while (!break) {
      // println(head, headChar)
      var fencePieces = ArrayBuffer.empty[Point]
      fencePieces.addOne(head)
      if (
        List('-', '_').contains(headChar) && (
          tail.contains(
            (twoDmap.goDirection(West, head), headChar)
          ) || tail.contains(
            (twoDmap.goDirection(East, head), headChar)
          )
        )
      ) {
        // find all horizontal and remove from tail
        var nextPiece = (twoDmap.goDirection(East, head), headChar)
        while (tail.contains(nextPiece)) {
          fencePieces.addOne(nextPiece._1)
          tail = tail diff List(nextPiece)
          nextPiece = (twoDmap.goDirection(East, nextPiece._1), headChar)
        }
        nextPiece = (twoDmap.goDirection(West, head), headChar)
        while (tail.contains(nextPiece)) {
          fencePieces.addOne(nextPiece._1)
          tail = tail diff List(nextPiece)
          nextPiece = (twoDmap.goDirection(West, nextPiece._1), headChar)
        }
      } else if (
        List('[', ']').contains(headChar) && (
          tail.contains(
            (twoDmap.goDirection(North, head), headChar)
          ) || tail.contains(
            (twoDmap.goDirection(South, head), headChar)
          )
        )
      ) {
        // find all vertical and remove from tail
        var nextPiece = (twoDmap.goDirection(South, head), headChar)
        while (tail.contains(nextPiece)) {
          fencePieces.addOne(nextPiece._1)
          tail = tail diff List(nextPiece)
          nextPiece = (twoDmap.goDirection(South, nextPiece._1), headChar)
        }
        nextPiece = (twoDmap.goDirection(North, head), headChar)
        while (tail.contains(nextPiece)) {
          fencePieces.addOne(nextPiece._1)
          tail = tail diff List(nextPiece)
          nextPiece = (twoDmap.goDirection(North, nextPiece._1), headChar)
        }
      }
      // println(fencePieces.sorted)
      numSides += 1
      if (tail.length > 0) {
        headTuple = tail.head
        head = headTuple._1
        headChar = headTuple._2
        tail = tail.tail
      } else {
        break = true
      }
    }
    numSides
  }

  def perimeter(plot: Set[Point]): List[(Point, Char)] = {
    plot.toList.flatMap(square => List(
      (twoDmap.goDirection(West, square), '['),
      (twoDmap.goDirection(East, square), ']'),
      (twoDmap.goDirection(North, square), '-'),
      (twoDmap.goDirection(South, square), '_'),
    )).filter(square => !plot.contains(square._1))
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

