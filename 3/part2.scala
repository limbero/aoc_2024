object Day3Part2 {
  def main(args: Array[String]): Unit = {
    val bufferedSource = io.Source.fromFile("data.txt")
    val lines = bufferedSource.getLines.toList
    val re = """mul\(\d{1,3},\d{1,3}\)""".r

    val doAreas = lines.mkString("").split("don't()").toList
    val goHere = (
        doAreas.take(1) :::
        doAreas
          .drop(1)
          .map(
            dontArea => {
              val maybeDo = dontArea.split("do()", 2)
              if (maybeDo.length > 1) {
                maybeDo(1)
              } else {
                Nil
              }
    })).mkString("")
    println(re.findAllIn(goHere).map(mul => {
      mul.slice(4,mul.length-1).split(",").map(_.toInt).reduce((a,b) => a*b)
    }).sum)
  }
}