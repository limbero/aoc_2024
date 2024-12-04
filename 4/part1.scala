object Day4Part1 {
  def main(args: Array[String]): Unit = {
    val bufferedSource = io.Source.fromFile("data.txt")
    val lines = bufferedSource.getLines.toList
    val re = """XMAS""".r

    val backforth = lines
      .map(line =>
        re.findAllIn(line).length + re.findAllIn(line.reverse).length
      )
      .sum

    val updown = lines.transpose
      .map(_.mkString(""))
      .map(line =>
        re.findAllIn(line).length + re.findAllIn(line.reverse).length
      )
      .sum

    val numberOfDiagonals = Range.inclusive(1, (lines.length * 2) - 1).zipWithIndex

    val southeastnorthwest = numberOfDiagonals
      .map((_, idx) => {
        if (idx < lines.length) {
          val dropped = lines.drop(lines.length - idx - 1)
          dropped.zipWithIndex
            .map((_, jdx) => {
              lines(dropped.length - jdx - 1)(lines.length - jdx - 1)
            })
            .mkString("")
        } else {
          val newIdx = idx - lines.length
          val dropped = lines.take(numberOfDiagonals.length - idx)
          dropped.zipWithIndex
            .map((_, jdx) => {
              lines(lines.length - jdx - 1)(dropped.length - jdx - 1)
            })
            .mkString("")
        }
      }).map(line =>
        re.findAllIn(line).length + re.findAllIn(line.reverse).length
      )
      .sum

    val mirrored = lines.reverse
    val southwestnortheast = numberOfDiagonals
      .map((_, idx) => {
        if (idx < lines.length) {
          val dropped = mirrored.drop(lines.length - idx - 1)
          dropped.zipWithIndex
            .map((_, jdx) => {
              mirrored(dropped.length - jdx - 1)(lines.length - jdx - 1)
            })
            .mkString("")
        } else {
          val newIdx = idx - lines.length
          val dropped = mirrored.take(numberOfDiagonals.length - idx)
          dropped.zipWithIndex
            .map((_, jdx) => {
              mirrored(lines.length - jdx - 1)(dropped.length - jdx - 1)
            })
            .mkString("")
        }
      }).map(line =>
        re.findAllIn(line).length + re.findAllIn(line.reverse).length
      )
      .sum
    println(backforth + updown + southeastnorthwest + southwestnortheast)
  }
}
