package aoc2024.`8`

import scala.collection.mutable.ArrayBuffer

object Day8Part2 {
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

    locsMap.foreach { case (_, locs) => pairwise(locs, antinodes, mapSize) }

    val validAntinodes = antinodes.filter { case (xdx, ydx) => xdx >= 0 && ydx >= 0 && xdx < mapSize && ydx < mapSize}.toSet

    lines.zipWithIndex.foreach { case (line, ydx) =>
      println(line.zipWithIndex.map { case (char, xdx) =>
        if (validAntinodes.contains((xdx, ydx))) { '#' } else { char }
      }.mkString)
    }
    println("")
    println(validAntinodes.size)
  }

  def pairwise(locs: List[(Int, Int)], antinodes: ArrayBuffer[(Int,Int)], mapSize: Int): Unit = {
    if (locs.length == 0) return
    val head :: tail = locs

    tail.foreach { loc =>
      antinodesForPair(head, loc, antinodes, mapSize)
    }
    pairwise(tail, antinodes, mapSize)
  }

  def antinodesForPair(a: (Int, Int), b: (Int, Int), antinodes: ArrayBuffer[(Int,Int)], mapSize: Int): Unit = {
    val xDiff = a._1 - b._1
    val yDiff = a._2 - b._2

    var res = (0,0)
    var mul = 0
    while (res._1 >= 0 && res._2 >= 0 && res._1 < mapSize && res._2 < mapSize) {
      res = (a._1 + (xDiff * mul), a._2 + (yDiff * mul))
      antinodes.addOne(res)
      mul += 1
    }

    res = (0,0)
    mul = 0
    while (res._1 >= 0 && res._2 >= 0 && res._1 < mapSize && res._2 < mapSize) {
      res = (b._1 - (xDiff * mul), b._2 - (yDiff * mul))
      antinodes.addOne(res)
      mul += 1
    }

    antinodes.addAll(List((a._1 + xDiff, a._2 + yDiff),(b._1 - xDiff, b._2 - yDiff)))
  }
}
