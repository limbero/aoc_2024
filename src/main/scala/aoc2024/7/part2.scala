package aoc2024.`7`

object Day7Part2 {
  def main(args: Array[String]): Unit = {
    val prefix = "src/main/resources/7/"
    val bufferedSource = io.Source.fromFile(prefix + "data.txt")
    val lines = bufferedSource.getLines.toList

    println(lines.map { line => 
      val resultString :: operandsString :: _ = line.split(": ", 2).toList
      val result = resultString.toLong
      val operands = operandsString.split(" ").map(_.toLong).toList
      val res = calculate(operands, result)
      res
    }.sum)
  }

  def calculate(operands: List[Long], targetResult: Long): Long = {
    val nextOperand :: operandsTail = operands

    val nextPlus = calculateWithNext(nextOperand, operandsTail, '+', targetResult)
    val nextMultiply = calculateWithNext(nextOperand, operandsTail, '*', targetResult)
    val nextConcat = calculateWithNext(nextOperand, operandsTail, '|', targetResult)

    if (nextMultiply == targetResult) {
      nextMultiply
    } else if (nextPlus == targetResult) {
      nextPlus
    } else if (nextConcat == targetResult) {
      nextConcat
    } else {
      0
    }
  }

  def calculateWithNext(prevResult: Long, operands: List[Long], operator: Char, targetResult: Long): Long = {
    val nextOperand :: operandsTail = operands
    val newResult = if (operator == '*') {
      prevResult * nextOperand
    } else if (operator == '+') {
      prevResult + nextOperand
    } else if (operator == '|') {
      (prevResult.toString + "" + nextOperand.toString).toLong
    } else {
      0
    }
    if (operandsTail.length == 0) {
      return newResult
    } else {
      val nextPlus = calculateWithNext(newResult, operandsTail, '+', targetResult)
      val nextMultiply = calculateWithNext(newResult, operandsTail, '*', targetResult)
      val nextConcat = calculateWithNext(newResult, operandsTail, '|', targetResult)

      if (nextMultiply == targetResult) {
        nextMultiply
      } else if (nextPlus == targetResult) {
        nextPlus
      } else if (nextConcat == targetResult) {
        nextConcat
      } else {
        0
      }
    }
  }
}
