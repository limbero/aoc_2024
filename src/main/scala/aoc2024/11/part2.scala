package aoc2024.`11`

import scala.collection.mutable.HashMap

object Day11Part2 {
  var memoMap = HashMap.empty[Long, List[Long]]
  var functionMemoMap = HashMap.empty[String, Long]

  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/11/"
    val line = io.Source.fromFile(prefix + "data.txt").getLines.next()

    val nums = line.split(" ").map(_.toLong).toList

    memoMap.addOne((0, List(1)))

    Range(0, 75).foreach(r => println((r+1) + ": " + nums.map(num => recurseOne(num, r)).sum))
  }
  
  def recurseOne(num: Long, count: Int): Long = {
    val newNums = if (memoMap.contains(num)) {
      memoMap(num)
    } else if (num.toString.length % 2 == 0) {
      val (left, right) = num.toString.splitAt(num.toString.length / 2)
      val newNumTemp = List(left.toLong, right.toLong)
      memoMap.addOne((num, newNumTemp))
      newNumTemp
    } else {
      val newNumTemp = List[Long](num * 2024)
      memoMap.addOne((num, newNumTemp))
      newNumTemp
    }
    if (count == 0) {
      newNums.length.toLong
    } else {
      newNums.map { newNum =>
        val nextCount = count - 1
        val key = newNum + "," + nextCount
        if (functionMemoMap.contains(key)) {
          // println(newNum + ", " + nextCount + ": " + functionMemoMap(newNum, nextCount) + " (cached) or actually " + recurseOne(newNum, nextCount))
          // println("")
          // println("cache: (" + newNum + "," + nextCount + "," + functionMemoMap(key) + ")")
          // println(newNum, nextCount, recurseOne(newNum, nextCount))
          // println("")
          functionMemoMap(key)
        } else {
          val nextNumTemp = recurseOne(newNum, nextCount)
          // println(newNum, nextCount, nextNumTemp)
          functionMemoMap.addOne((key, nextNumTemp))
          nextNumTemp
        }
      }.sum
    }
  }
}

