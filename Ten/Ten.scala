object Ten {
  val usage = """
    Usage: Ten <1-10>
  """
  def main(args: Array[String]) {
    if (args.length != 1) println(usage)
    else {
      val begin = System.currentTimeMillis()

      val arglist = args.toList
      val func = ArgMatch(arglist.head)
      println(func)

      println(System.currentTimeMillis() - begin)
    }
  }


  def ArgMatch(arg: String): Any = arg match {
    case "1" => One
    case "2" => Two
    case "3" => "Three"
    case "4" => "Four"
    case "5" => "Five"
    case "6" => "Six"
    case "7" => "Seven"
    case "8" => "Eight"
    case "9" => "Nine"
    case "10" => "Ten"
    case _ => usage    
  }


  def One(): Long = {
    val ceil = 1000

    // This is much faster than the list comprehension solution but it will hit
    // recursion depth errors.
    def Mult3or5_rec(n: Long): Long = {
      if (n >= ceil) 0
      else if (n%3 == 0 || n%5 == 0) Mult3or5_rec(n+1) + n
      else Mult3or5_rec(n+1)
    }
    
    def Mult3or5_comp(n: Long): List[Long] = {
      for(i <- List.range(0, n) if i%3 == 0 || i%5 == 0) yield i
    }
    // Mult3or5_rec(1)
    Mult3or5_comp(ceil).sum
  }


  def Two(): Long = {
    val ceil = 4000000
    var sum: Long = 2

    def FibSum_lazy(): Long = {
      lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map {n => n._1 + n._2}
      fib.takeWhile {x => x <= ceil} .foldLeft(0) {(acc, x) => if (x%2 == 0) acc+x else 0+acc}
    }

    def FibSum_rec(last_2: Int, last_1: Int): Long = { 
      if (last_2+last_1 > ceil) sum
      else {
        if ((last_2+last_1)%2 == 0) sum += last_2+last_1
        FibSum_rec(last_1, last_2+last_1)
      }
    }
    FibSum_lazy()
    // FibSum_rec(1, 2)
  }


  // def Three(): Int = {
  //   def PrimeFactors(n: Long): List[Int] = {

  //   }
  // }
}