package aoc2024.`6`

object Day6Part2 {
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

    val (boardWithXes, _) = patrolGuard(lines, guardx, guardy)
    boardWithXes.foreach(line => println(line))
    println("")
    var numLoops = 0
    var numTests = 0

    boardWithXes.zipWithIndex.foreach{ case (line, ydx) =>
      line.zipWithIndex.foreach{ case (char, xdx) =>
        if (char == 'X') {
          numTests += 1
          val (_, hasLoop) = patrolGuard(replaceChar(lines, xdx, ydx, '#'), guardx, guardy)
          if (hasLoop) {
            numLoops += 1
            println("tested " + numTests + " of 4789, loops found: " + numLoops)
          }
        }
      }
    }
    println("")
    println(numLoops)
  }

  def replaceChar(board: List[String], xCoord: Int, yCoord: Int, char: Char): List[String] = {
    board.zipWithIndex.map { case (line, ydx) =>
      if (ydx == yCoord) {
        line.zipWithIndex
          .map { case (value, xdx) =>
            if (xdx == xCoord) { char }
            else { value }
          }
          .mkString("")
      } else { line }
    }
  }

  def patrolGuard(board: List[String], guardx: Int, guardy: Int): (List[String], Boolean) = {
    var hasLoop = false
    var boardCopy = board
    var guardxCurrent = guardx
    var guardyCurrent = guardy
    var numRotations = 0
    val buf = scala.collection.mutable.ArrayBuffer.empty[String]

    try {
      while (true) {
        boardCopy = replaceChar(boardCopy, guardxCurrent, guardyCurrent, 'X')
        val newLoc = guardxCurrent + "," + guardyCurrent + "," + (numRotations % 4)
        if (buf.contains(newLoc)) {
          hasLoop = true
          throw AllDone
        }
        buf += newLoc
        if (guardyCurrent == 0) {
          throw AllDone
        }
        if (boardCopy(guardyCurrent - 1)(guardxCurrent) == '#') {
          boardCopy = rotateLeft(boardCopy)
          numRotations += 1
          val oldx = guardxCurrent
          guardxCurrent = guardyCurrent
          guardyCurrent = board.length - oldx - 1
        } else {
          guardyCurrent -= 1
        }
      }
    } catch {
      case AllDone => {
        // println(boardCopy.mkString("").filter(_ == 'X').length)
      }
    }
    numRotations = numRotations % 4
    while (numRotations > 0) {
      boardCopy = rotateRight(boardCopy)
      numRotations -= 1
    }
    (boardCopy, hasLoop)
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
