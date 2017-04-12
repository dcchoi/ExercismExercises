object SumOfMultiples {
  def sumOfMultiples(factors: Set[Int], limit: Int): Int = {
    sumList(gatherSumOfMultipleList(factors.toList,limit,List.empty))
  }

  //auxiliary function to make tail recursive
  private def gatherSumOfMultipleList(factors: List[Int], limit: Int, sumGathered: List[Int]): List[Int] = {
    factors match {
      case Nil => sumGathered
      case head :: tail => gatherSumOfMultipleList(tail, limit, sumGathered.union(Range(head, limit, head)).distinct)
    }
  }
  private def sumList(someList: List[Int]): Int = {

    //auxiliary function to make tail recursive
    def accumulator(someList: List[Int], accum: Int): Int = {
      someList match {
        case Nil => accum
        case head :: tail => accumulator(tail, accum + head)
      }
    }
    accumulator(someList,0)
  }
}

