package aoc2024.`5`

object Day5Part2 {
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

    val rulesMap = rules
      .map(str => str.split("\\|").map(_.toInt).toList)
      .groupBy(_(0))
      .map { case (key, values) => (key, values.map(value => value(1))) }

    println(
      updates
        .map(update => {
          val updateInts = update.split(",").map(_.toInt).toList
          val reversedUpdateInts = updateInts.reverse
          if (
            reversedUpdateInts.zipWithIndex.toList
              .filter { case (page: Int, idx: Int) => {
                reversedUpdateInts
                  .drop(idx + 1)
                  .filter((pageInTail: Int) => {
                    rulesMap.contains(page) && rulesMap(page)
                      .contains(pageInTail)
                  })
                  .length > 0
              }}.length > 0
          ) {
            updateInts
          } else { Nil }
        })
        .filter(_.length > 0)
        .map(update => {
          val sorted = sort(update, rulesMap)
          println(sorted)
          sorted
        })
        .map(update => update((update.length - 1) / 2))
        .sum
    )
  }

  def insert[T](list: List[T], i: Int, value: T): List[T] = {
    val (front, back) = list.splitAt(i)
    front ++ List(value) ++ back
  }

  def sort(update: List[Int], rulesMap: Map[Int, List[Int]]): List[Int] = {
    // map goes from before to after
    val sorted = update.foldLeft(List[Int]())((sortedPages, currentPage) => {
      val insertionIndex = sortedPages.zipWithIndex.find { case (sortedPage, idx) => {
        // if the current page should not go before that is our item
        isBefore(currentPage, sortedPage, rulesMap, sortedPages)
      }} match {
        case Some((_, index)) => index
        case _                => sortedPages.length
      }
      insert(sortedPages, insertionIndex, currentPage)
    })
    // println(sorted)
    sorted
  }

  def isBefore(
      toInsert: Int,
      alreadyInList: Int,
      rulesMap: Map[Int, List[Int]],
      sortedPages: List[Int]
  ): Boolean = {
    if (!rulesMap.contains(toInsert)) {
      false
    } else if (rulesMap(toInsert).contains(alreadyInList)) {
      true
    } else {
      val rightSet = rulesMap(toInsert).toSet
      // if the rightSet has no elements in common with the sortedPages, we are at the end maybe?
      if ((rightSet & sortedPages.toSet).size == 0) {
        false
      } else {
        val newRulesMap = rulesMap.map {
          case (`toInsert`, afterList) => {
            (
              toInsert,
              afterList.flatMap(afterListItem =>
                if (rulesMap.contains(afterListItem)) {
                  rulesMap(afterListItem)
                } else { Nil }
              )
            )
          }
          case x => x
        }
        isBefore(toInsert, alreadyInList, newRulesMap, sortedPages)
      }
    }
  }
}
