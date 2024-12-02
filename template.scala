object Solver {
  def main(args: Array[String]): Unit = {
    val bufferedSource = io.Source.fromFile("data.txt")
    val lines = bufferedSource.getLines.toList
    lines.map(line => line.split(" ")).foreach(println)
  }
}