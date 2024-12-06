package aoc2024.`5`

object Day5Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/5/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val lines = bufferedSource.getLines.toList
    val transitionLine =
      lines.zipWithIndex.find { case (line, _) => line == "" } match {
        case Some((_, index)) => index
        case _                => 0
      }
    val (rules, updates) =
      lines.patch(transitionLine, Nil, 1).splitAt(transitionLine)

    val rulesMap = rules.map(str => str.split("\\|").map(_.toInt).toList)
      .groupBy(_(0))
      .map{case (key, values) => (key, values.map(value => value(1)))}


    println(updates.map(update => {
      val reversedUpdateInts = update.split(",").map(_.toInt).reverse
      if (
        reversedUpdateInts.zipWithIndex.toList
          .filter { case (page: Int, idx: Int) => {
            reversedUpdateInts
              .drop(idx + 1)
              .filter((pageInTail: Int) => {
                rulesMap.contains(page) && rulesMap(page).contains(pageInTail)
              })
              .length > 0
          }}
          .length == 0
      ) {
        reversedUpdateInts((reversedUpdateInts.length - 1) / 2)
      }
      else { 0 }
    }).sum)
  }
}
