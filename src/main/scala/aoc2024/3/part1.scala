package aoc2024

object Day3Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/3/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val lines = bufferedSource.getLines.toList
    val re = """mul\(\d{1,3},\d{1,3}\)""".r
    println(lines.flatMap(line => {
      re.findAllIn(line).toList
    }).map(mul => {
      mul.slice(4,mul.length-1).split(",").map(_.toInt).reduce((a,b) => a*b)
    }).sum)
  }
}