package aoc2024.`9`

import scala.collection.mutable.ArrayBuffer

object Day9Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/9/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val diskmap = bufferedSource.getLines.next()

    val disk = diskmap.map(_.asDigit).zipWithIndex.flatMap { case (num, idx) =>
      if (idx % 2 == 0) {
        List.fill(num)((idx / 2).toString)
      } else {
        List.fill(num)(".")
      }
    }.toList

    val dots = disk.filter(_ == ".").toList

    println(diskmap)
    println("")
    println(disk)
    val compacted = compact(disk, dots)
    println("")
    println(compacted)
    println("")
    val checksumParts = compacted.filter(_ != ".").map(_.toLong).zipWithIndex.map { case (a,b) => a*b }
    println(checksumParts)
    println("")
    println(checksumParts.sum)
  }

  def compact(disk: List[String], dots: List[String]): List[String] = {
    if (disk.endsWith(dots)) {
      disk
    } else {
      val lastNum = disk.zipWithIndex.findLast { case (str, idx) => str != "." }.get
      val firstEmpty = disk.zipWithIndex.find { case (str, idx) => str == "." }.get
      val newDisk = replaceStringAt(replaceStringAt(disk, ".", lastNum._2), lastNum._1, firstEmpty._2)
      // println(newDisk)
      compact(newDisk, dots)
    }
  }

  def replaceStringAt(list: List[String], str: String, idx: Int): List[String] = {
    val (firstHalf, secondHalf) = list.splitAt(idx)
    firstHalf ::: List(str) ::: secondHalf.drop(1)
  }
}
