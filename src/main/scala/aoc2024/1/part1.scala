package aoc2024.`1`

object Day1Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/1/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val lines = bufferedSource.getLines.map(line => line.split("\\s+").toList).toList
    val left = lines.map(part => part(0)).map(_.toInt).sorted.toList
    val right = lines.map(part => part(1)).map(_.toInt).sorted.toList
    // println(left.mkString("[", ", ", "]"))

    println(left.zip(right).map { case (a,b) => (a-b).abs }.sum)
  }
}