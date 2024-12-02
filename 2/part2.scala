object Day2Part2 {
  def main(args: Array[String]): Unit = {
    val bufferedSource = io.Source.fromFile("data.txt")
    val reports = bufferedSource.getLines
      .map(line => line.split("\\s+").map(_.toInt).toList)
      .toList
    println(
      reports
        .filter(report => {
          isSafe(report) || report.zipWithIndex
            .map((_, idx) => {
              val rep2 = report.patch(idx, Nil, 1)
              isSafe(rep2)
            })
            .filter(_ == true)
            .length > 0
        })
        .length
    )
  }

  def isSafe(report: List[Int]): Boolean = {
    val diffs = report.zip(report.drop(1)).map((a, b) => a - b)

    val zeroes = diffs.filter(_ == 0)
    val pluses = diffs.filter(_ > 0)
    val minuses = diffs.filter(_ < 0)

    val abs = diffs.map(_.abs).filter(_ > 3)

    abs.length == 0 && zeroes.length == 0 && (pluses.length == 0 || minuses.length == 0)
  }
}
