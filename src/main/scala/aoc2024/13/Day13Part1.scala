package aoc2024.`13`

import aoc2024.util.{Point}

object Day13Part1 {
  def main(args: Array[String]): Unit = {
    val dayNumber = this.getClass().getPackageName().split('.')(1)
    val prefix = f"src/main/resources/$dayNumber/"
    val lines = io.Source.fromFile(prefix + "data.txt").getLines.toList.zipWithIndex

    val as = getLinesModuloFour(lines, 0)
    val bs = getLinesModuloFour(lines, 1)
    val prizes = getLinesModuloFour(lines, 2)

    val COST_A = 3
    val COST_B = 1
    val totalCost = as.zipWithIndex.map { case (a, idx) =>
      val b = bs(idx)
      val prize = prizes(idx)

      var cheapestSolution = 401

      for (adx <- 0 to 100){
        for (bdx <- 0 to 100){
          val result = Point(
            a.x * adx + b.x * bdx,
            a.y * adx + b.y * bdx,
          )
          val cost = COST_A * adx + COST_B * bdx
          if (result == prize && cost < cheapestSolution) {
            cheapestSolution = cost
          }
        }
      }
      println(cheapestSolution)
      cheapestSolution
    }.filter(_ != 401).sum
    println("")
    println(totalCost)
  }

  def getLinesModuloFour(lines: List[(String, Int)], modulo: Int): List[Point] = {
    lines
      .filter(line => line._2 % 4 == modulo)
      .map(_._1)
      .map(
        _
          .split(": ")(1)
          .split(", ").toList
          .map(_.drop(2).toInt)
      )
      .map(loc => Point(loc(0), loc(1)))
  }
}

