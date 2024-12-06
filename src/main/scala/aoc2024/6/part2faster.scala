package aoc2024.`6`

import scala.collection.mutable.ArrayBuffer

object Day6Part2Faster {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/6/"
    val bufferedSource = io.Source.fromFile(prefix + "testdata.txt")
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

    val boardBuffer = ArrayBuffer.empty[ArrayBuffer[Char]]

    for (y <- 0 until lines.length) {
      boardBuffer.addOne(ArrayBuffer.empty[Char])
      for (x <- 0 until lines.length) {
        boardBuffer(y).addOne(lines(y)(x))
      }
      // println(boardBuffer(y).mkString)
    }
    // println("")

    var numLoops = 0
    var numTests = 0
    val searchSpace = boardWithXes.mkString("").filter(_ == 'X').length
  
    boardWithXes.zipWithIndex.foreach{ case (line, ydx) =>
      line.zipWithIndex.foreach{ case (char, xdx) =>
        if (char == 'X') {
          val tempBuffer = copyBoard(boardBuffer)
          tempBuffer(ydx)(xdx) = '#'
          val hasLoop = patrolFast(tempBuffer, guardx, guardy, false)

          numTests += 1
          if (hasLoop) {
            // println("tested " + numTests + " of " + searchSpace + ", loops found: " + numLoops)
            // println(xdx + "," + ydx)
            println(numLoops)
            numLoops += 1
          }
        }
      }
    }
    println("")
    println(numLoops)
  }

  def copyBoard(board: ArrayBuffer[ArrayBuffer[Char]]): ArrayBuffer[ArrayBuffer[Char]] = {
    val boardBuffer = ArrayBuffer.empty[ArrayBuffer[Char]]
    for (y <- 0 until board.length) {
      boardBuffer.addOne(ArrayBuffer.empty[Char])
      for (x <- 0 until board.length) {
        boardBuffer(y).addOne(board(y)(x))
      }
    }
    boardBuffer
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

  def patrolFast(board: ArrayBuffer[ArrayBuffer[Char]], guardx: Int, guardy: Int, debug: Boolean): Boolean = {
    var hasLoop = false
    var boardCopy = copyBoard(board)
    var guardDirection = 0 // 0 up, 1 right, 2 down, 3 left
    var break = false

    var guardxCurrent = guardx
    var guardyCurrent = guardy
    val buf = ArrayBuffer.empty[String]

    try {
      while (!break) {
        boardCopy(guardyCurrent)(guardxCurrent) = 'X'

        val newLoc = guardxCurrent + "," + guardyCurrent + "," + (guardDirection % 4)
        if (debug) {
          println(newLoc)
        }
        if (buf.contains(newLoc)) {
          println(newLoc)
          println(buf)
          println("")
          hasLoop = true
          break = true
        }
        if (!break) {
          buf += newLoc

          val nextSquare = guardDirection match {
            case 0 => boardCopy(guardyCurrent - 1)(guardxCurrent)
            case 1 => boardCopy(guardyCurrent)(guardxCurrent + 1)
            case 2 => boardCopy(guardyCurrent + 1)(guardxCurrent)
            case 3 => boardCopy(guardyCurrent)(guardxCurrent - 1)
          }
          if (nextSquare == '#') {
            guardDirection = (guardDirection + 1) % 4
          }

          if (guardDirection == 0) {
            guardyCurrent -= 1
          } else if (guardDirection == 1) {
            guardxCurrent += 1
          } else if (guardDirection == 2) {
            guardyCurrent += 1
          } else {
            guardxCurrent -= 1
          }

          if (guardyCurrent < 0 || guardyCurrent == boardCopy.length || guardxCurrent < 0 || guardxCurrent == boardCopy.length) {
            break = true
          }
        }
      }
    } catch {
      case _: Throwable => {}
    }
    
    if (debug) {
      println("")
      for (y <- 0 until boardCopy.length) {
        println(boardCopy(y).mkString)
      }
      println("")
      for (y <- 0 until board.length) {
        println(board(y).mkString)
      }
      println("")
    }

    hasLoop
  }

  def patrolGuard(board: List[String], guardx: Int, guardy: Int): (List[String], Boolean) = {
    var hasLoop = false
    var boardCopy = board
    var guardxCurrent = guardx
    var guardyCurrent = guardy
    var numRotations = 0
    val buf = ArrayBuffer.empty[String]

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
          .mkString
      }
    }
  }
}
