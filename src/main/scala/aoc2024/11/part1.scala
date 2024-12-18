package aoc2024.`11`

import scala.collection.mutable.ArrayBuffer

object Day11Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/11/"
    val line = io.Source.fromFile(prefix + "data.txt").getLines.next()

    val nums = line.split(" ").map(_.toLong).toList

    println(recurse(nums, 1, 25).length)
  }

  def recurse(nums: List[Long], count: Int, maxCount: Int): List[Long] = {
    val newNums = nums.flatMap { num =>
      if (num == 0) {
        List[Long](1)
      } else if (num.toString.length % 2 == 0) {
        val (left, right) = num.toString.splitAt(num.toString.length / 2)
        List(left.toLong, right.toLong)
      } else {
        List[Long](num * 2024)
      }
    }
    if (count == maxCount) {
      newNums
    } else {
      recurse(newNums, count + 1, maxCount)
    }
  }
}

