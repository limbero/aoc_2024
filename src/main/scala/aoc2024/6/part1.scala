package aoc2024.`6`

object AllDone extends Exception {}

object Day6Part1 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/6/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val lines = bufferedSource.getLines.toList

    var guardx = -1
    var guardy = -1
    lines.zipWithIndex.map { case (line, ydx) =>
      line.zipWithIndex.map { case (char, xdx) =>
        if (char == '^') {
          guardx = xdx
          guardy = ydx
        }
        (char, xdx)
      }
    }
    var board = lines
    try {
      while (true) {
        board = board.zipWithIndex.map { case (line, ydx) =>
          if (ydx == guardy) {
            line.zipWithIndex
              .map { case (value, xdx) =>
                if (xdx == guardx) { 'X' }
                else { value }
              }
              .mkString("")
          } else { line }
        }
        if (guardy == 0) {
          throw AllDone
        }
        if (board(guardy - 1)(guardx) == '#') {
          board = rotateLeft(board)
          val oldx = guardx
          guardx = guardy
          guardy = board.length - oldx - 1
        } else {
          guardy -= 1
        }
      }
    } catch {
      case AllDone => {
        println(board.mkString("").filter(_ == 'X').length)
      }
    }
  }

  def rotateLeft(board: List[String]): List[String] = {
    rotateRight(rotateRight(rotateRight(board)))
  }

  def rotateRight(board: List[String]): List[String] = {
    board.zipWithIndex.map {
      case (_, idx) => {
        board.zipWithIndex
          .map {
            case (_, jdx) => {
              board(board.length - jdx - 1)(idx)
            }
          }
          .mkString("")
      }
    }
  }
}
