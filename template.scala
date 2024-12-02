object Solver {
  def main(args: Array[String]): Unit = {
    val bufferedSource = io.Source.fromFile("data.txt")
    val lines = bufferedSource.getLines.map(line => line.split("\\s+").toList).toList
  }
}