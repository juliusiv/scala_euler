object Ten {
  val usage = """
    Usage: Ten <1-10>
  """

  lazy val Sieve: Stream[Int] = 2 #:: Stream.from(3).filter {
    x => Sieve.takeWhile {i => i*i <= x}.forall(x%_ != 0) }

  def main(args: Array[String]) {
    if (args.length != 1) println(usage)
    else {
      val begin = System.currentTimeMillis()

      val arglist = args.toList
      val func = ArgMatch(arglist.head)
      println(func)

      println("Time: " + (System.currentTimeMillis() - begin))
    }
  }


  def ArgMatch(arg: String): Any = arg match {
    case "1" => One
    case "2" => Two
    case "3" => Three
    case "4" => Four
    case "5" => Five
    case "6" => Six
    case "7" => Seven
    case "8" => Eight
    case "9" => "Nine"
    case "10" => "Ten"
    case _ => usage    
  }

  /*
    Find the sum of all the multiples of 3 or 5 below 1000.
    Answer: 233168
  */
  def One(): Long = {
    val ceil = 1000

    // This is much faster than the list comprehension solution but it will hit
    // recursion depth errors.
    def Mult3or5_rec(n: Long): Long = {
      if (n >= ceil) 0
      else if (n%3 == 0 || n%5 == 0) Mult3or5_rec(n+1) + n
      else Mult3or5_rec(n+1)
    }
    
    // Uses a comprehension to get all the multiples of 3 or 5 and then sums
    // them.
    def Mult3or5_comp(n: Long): Long = {
      List.range(0, n).filter { x => x%3 == 0 || x%5 == 0 }.sum
    }
    // Mult3or5_rec(1)
    Mult3or5_comp(ceil)
  }


  /*
    Find the sum of the even-valued terms of the Fibonacci sequence whose values
    do not exceed four million.
    Answer: 4613732
  */
  def Two(): Long = {
    val ceil = 4000000
    var sum: Long = 2

    // Uses a Stream to create the Fibonacci sequence and then accumulates all
    // the even values.
    def FibSum_lazy(): Long = {
      lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map {
        n => n._1 + n._2
      }
      fib.takeWhile {x => x <= ceil} .foldLeft(0) {
        (acc, x) => if (x%2 == 0) acc+x else 0+acc
      }
    }

    // Recursively finds the next term of the Fibonacci sequence and adds them
    // up as they go.
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


  /*
    What is the largest prime factor of the number 600851475143?
    Answer: 6857
  */
  def Three(): Long = {
    // Relies on the fact that once we've found a prime that is not a factor,
    // the remaining prime factors must be larger than the last prime factors.
    def PrimeFactors(n: Long, primes: Stream[Int]): Long = {
      if (n == primes.head) n
      else if (n%primes.head == 0) PrimeFactors(n/primes.head, primes)
      else PrimeFactors(n, primes.tail)
    }
    PrimeFactors(600851475143L, Sieve)
  }


  /*
    Find the largest palindrome made from the product of two 3-digit numbers.
    Answer: 906609
  */
  def Four(): Int = {
    val ints: List[Int] = List.range(1, 1000)
    val mults = for ( i <- ints) yield ints.map(i*_)
    val palindromes = mults.flatten
      .filter {x => x.toString == x.toString.reverse}
    palindromes.max
  }


  /*
    What is the smallest positive number that is evenly divisible by all of the 
    numbers from 1 to 20?
    Answer: 232792560
  */
  def Five(): Int = {
    val div = List.range(1, 21)
    Stream.from(div.last).toIterator.find {x => div.forall(x%_ == 0)}.get
  }


  /*
    Find the difference between the sum of the squares of the first one hundred
    natural numbers and the square of the sum.
    Answer: 25164150
  */
  def Six(): Long = {
    val nums: List[Long] = List.range(1, 101)
    val sum = nums.sum
    (sum*sum) - (nums.map {x => x*x}.foldLeft(0L) ( _+_ ))
  }


  /*
    What is the 10 001st prime number?
    Answer: 104743
  */
  def Seven(): Int = {
    Sieve.take(10001).last
  }


  /*
    Find the thirteen adjacent digits in the 1000-digit number that have the
    greatest product. What is the value of this product?
    Answer: 23514624000
  */
  def Eight(): Long = {
    val nums = io.Source.fromFile("eight.txt").mkString.toList.map(_.toLong-48L)

    def AdjacentProduct(xs: List[Long], depth: Int): List[Long] = {
      if (depth <= 0) xs
      else xs.grouped(13).map {x => x.foldLeft(1L)( _*_ )}.max :: 
        AdjacentProduct(xs.tail, depth-1)
    }
    AdjacentProduct(nums, 13).max
  }
}