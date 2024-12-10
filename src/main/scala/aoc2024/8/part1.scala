package aoc2024.`8`

import scala.collection.mutable.ArrayBuffer

object Day8Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/8/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val lines = bufferedSource.getLines.toList

    val locsMap  = lines.zipWithIndex.flatMap { case (line, ydx) =>
      line.zipWithIndex.filter { case (char, _) =>
        char != '.'
      }.map { case (char, xdx) =>
        char -> (xdx, ydx)
      }
    }.groupMap(_._1)(_._2)

    var antinodes = ArrayBuffer.empty[(Int, Int)]
    val mapSize = lines.length

    locsMap.foreach { case (_, locs) => pairwise(locs, antinodes) }

    val validAntinodes = antinodes.filter { case (xdx, ydx) => xdx >= 0 && ydx >= 0 && xdx < mapSize && ydx < mapSize}.toSet

    lines.zipWithIndex.foreach { case (line, ydx) =>
      println(line.zipWithIndex.map { case (char, xdx) =>
        if (validAntinodes.contains((xdx, ydx))) { '#' } else { char }
      }.mkString)
    }
    println("")
    println(validAntinodes.size)
  }

  def pairwise(locs: List[(Int, Int)], antinodes: ArrayBuffer[(Int,Int)]): Unit = {
    if (locs.length == 0) return
    val head :: tail = locs
    tail.foreach { loc =>
      antinodes.addAll(antinodesForPair(head, loc))
    }
    pairwise(tail, antinodes)
  }

  def antinodesForPair(a: (Int, Int), b: (Int, Int)): List[(Int, Int)] = {
    val xDiff = a._1 - b._1
    val yDiff = a._2 - b._2

    List((a._1 + xDiff, a._2 + yDiff),(b._1 - xDiff, b._2 - yDiff))
  }
}
