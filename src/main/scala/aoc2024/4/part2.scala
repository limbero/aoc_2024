package aoc2024.`4`

object Day4Part2 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/4/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val lines = bufferedSource.getLines.toList
    val reString =
      "(?=(M(\\D)M.{" + (lines.length - 1) + "}A.{" + (lines.length - 1) + "}S(\\D)S))"
    val re = reString.r

    val zero = lines.mkString("0")
    val ninety = rotate(lines).mkString("0")
    val oneeighty = rotate(rotate(lines)).mkString("0")
    val twoseventy = rotate(rotate(rotate(lines))).mkString("0")

    println(
      re.findAllIn(zero).length + re.findAllIn(ninety).length + re
        .findAllIn(oneeighty)
        .length + re.findAllIn(twoseventy).length
    )
  }

  def rotate(board: List[String]): List[String] = {
    board.zipWithIndex.map { case (_, idx) => {
      board.zipWithIndex
        .map { case (_, jdx) => {
          board(board.length - jdx - 1)(idx)
        }}
        .mkString("")
    }}
  }
}
