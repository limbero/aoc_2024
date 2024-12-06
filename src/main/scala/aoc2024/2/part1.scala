package aoc2024.`2`

object Day2Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/2/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val reports = bufferedSource.getLines.map(line => line.split("\\s+").map(_.toInt).toList).toList
    println(reports.filter(report => {
      val diffs = report.zip(report.drop(1)).map { case (a,b) => a-b }

      val zeroes = diffs.filter(_ == 0)
      val pluses = diffs.filter(_ > 0)
      val minuses = diffs.filter(_ < 0)

      val abs = diffs.map(_.abs).filter(_ > 3)

      abs.length == 0 && zeroes.length == 0 && (pluses.length == 0 || minuses.length == 0)
    }).length)
  }
}