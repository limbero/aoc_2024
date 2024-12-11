package aoc2024.`9`

import scala.collection.mutable.ArrayBuffer

object Day9Part2 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/9/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val diskmap = bufferedSource.getLines.next().map(_.asDigit).toList

    val files = diskmap.zipWithIndex.filter { case (num, idx) =>
      idx % 2 == 0
    }.map { case (a,b) => (a, b/2)}
    val gaps = diskmap.zipWithIndex.filter { case (num, idx) =>
      idx % 2 == 1
    }.map { case (a,b) => (a, -(b / 2 + 1))}

    val disk = diskmap.zipWithIndex.flatMap { case (num, idx) =>
      if (idx % 2 == 0) {
        List.fill(num)((idx / 2))
      } else {
        List.fill(num)(-(idx / 2 + 1))
      }
    }.toList

    val filesHead :: filesTail = files.reverse
    val compacted = compact(filesHead, filesTail, files, gaps, disk)
    println(compacted.zipWithIndex.filter(_._1 > 0).map { case (a,b) => a.toLong*b.toLong }.sum)
  }

  def toDiskString(diskInts: List[Int]): String = {
    diskInts.map(a => if (a < 0) { '.' } else { a.toString }).mkString
  }

  def compact(
    nextFile: (Int, Int),
    restOfFiles: List[(Int, Int)],
    allFiles: List[(Int, Int)],
    gaps: List[(Int, Int)],
    disk: List[Int]
  ): List[Int] = {
    // println(toDiskString(disk))
    // println("gaps:")
    // println(gaps)
    if (restOfFiles.length != 0) {
      // fit the file if possible
      val findTheGap = gaps.zipWithIndex.find(_._1._1 >= nextFile._1)
      val filesHead :: filesTail = restOfFiles
      if (findTheGap.isDefined && findTheGap.get._2.abs.toInt < nextFile._2) {
        val foundGap = findTheGap.get
        // check that gap is to the left of the actual index
        val reducedGap = (foundGap._1._1 - nextFile._1, foundGap._1._2)
        val indexToReplaceGap = disk.zipWithIndex.find(_._1 == foundGap._1._2).get._2
        val indexToReplaceNum = disk.zipWithIndex.findLast(_._1 == nextFile._2).get._2 - nextFile._1 + 1
        // println("indexToReplaceNum: " + indexToReplaceNum)
        compact(
          filesHead,
          filesTail,
          allFiles,
          replaceAt(gaps, reducedGap, foundGap._2),
          // disk
          // replaceRange(disk, )
          replaceRange(
            replaceRange(
              disk,
              List.fill(nextFile._1)(nextFile._2),
              indexToReplaceGap
            ),
            List.fill(nextFile._1)(-1),
            indexToReplaceNum
          )
        )
      } else {
        compact(filesHead, filesTail, allFiles, gaps, disk)
      }
    } else {
      disk
    }
  }

  def replaceAt[T](list: List[T], el: T, idx: Int): List[T] = {
    val (firstHalf, secondHalf) = list.splitAt(idx)
    firstHalf ::: List(el) ::: secondHalf.drop(1)
  }

  def replaceRange[T](list: List[T], elList: List[T], idx: Int): List[T] = {
    val (firstHalf, secondHalf) = list.splitAt(idx)
    firstHalf ::: elList ::: secondHalf.drop(elList.length)
  }

  // def compact(disk: List[String], dots: List[String]): List[String] = {
  //   if (disk.endsWith(dots)) {
  //     disk
  //   } else {
  //     val lastNum = disk.zipWithIndex.findLast { case (str, idx) => str != "." }.get
  //     val firstEmpty = disk.zipWithIndex.find { case (str, idx) => str == "." }.get
  //     val newDisk = replaceStringAt(replaceStringAt(disk, ".", lastNum._2), lastNum._1, firstEmpty._2)
  //     // println(newDisk)
  //     compact(newDisk, dots)
  //   }
  // }
}
