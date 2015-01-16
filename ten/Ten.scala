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
      val func = argMatch(arglist.head)
      println(func)

      println("Time: " + (System.currentTimeMillis() - begin))
    }
  }


  /*
    Parses the command line arguments to call the right functions.
  */
  def argMatch(arg: String): Any = arg match {
    case "1" => One
    case "2" => Two
    case "3" => Three
    case "4" => Four
    case "5" => Five
    case "6" => Six
    case "7" => Seven
    case "8" => Eight
    case "9" => Nine
    case "10" => Ten
    case _ => usage    
  }

  /*
    Find the sum of all the multiples of 3 or 5 below 1000.
    Answer: 233168
  */
  def One(): AnyVal = {
    val ceil = 1000

    // This is much faster than the list comprehension solution but it will hit
    // recursion depth errors.
    def multSumRec(n: Long): Long = {
      if (n >= ceil) 0
      else if (n%3 == 0 || n%5 == 0) multSumRec(n+1) + n
      else multSumRec(n+1)
    }
    
    // Uses a comprehension to get all the multiples of 3 or 5 and then sums
    // them.
    def multSumComp(n: Long): Long = {
      List.range(0, n).filter { x => x%3 == 0 || x%5 == 0 }.sum
    }
    // multSumRec(1)
    multSumComp(ceil)
  }


  /*
    Find the sum of the even-valued terms of the Fibonacci sequence whose values
    do not exceed four million.
    Answer: 4613732
  */
  def Two(): AnyVal = {
    val ceil = 4000000
    var sum: Long = 2

    // Uses a Stream to create the Fibonacci sequence and then accumulates all
    // the even values.
    def fibSumLazy(): AnyVal = {
      lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map {
        n => n._1 + n._2
      }
      fib.takeWhile {x => x <= ceil} .foldLeft(0) {
        (acc, x) => if (x%2 == 0) acc+x else 0+acc
      }
    }

    // Recursively finds the next term of the Fibonacci sequence and adds them
    // up as they go.
    def fibSumRec(last_2: Int, last_1: Int): AnyVal = { 
      if (last_2+last_1 > ceil) sum
      else {
        if ((last_2+last_1)%2 == 0) sum += last_2+last_1
        fibSumRec(last_1, last_2+last_1)
      }
    }
    fibSumLazy()
    // fibSumRec(1, 2)
  }


  /*
    What is the largest prime factor of the number 600851475143?
    Answer: 6857
  */
  def Three(): AnyVal = {
    // Relies on the fact that once we've found a prime that is not a factor,
    // the remaining prime factors must be larger than the last prime factors.
    def primeFactors(n: Long, primes: Stream[Int]): AnyVal = {
      if (n == primes.head) n
      else if (n%primes.head == 0) primeFactors(n/primes.head, primes)
      else primeFactors(n, primes.tail)
    }
    primeFactors(600851475143L, Sieve)
  }


  /*
    Find the largest palindrome made from the product of two 3-digit numbers.
    Answer: 906609
  */
  def Four(): AnyVal = {
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
  def Five(): AnyVal = {
    val div = List.range(1, 21)
    Stream.from(div.last).toIterator.find {x => div.forall(x%_ == 0)}.get
  }


  /*
    Find the difference between the sum of the squares of the first one hundred
    natural numbers and the square of the sum.
    Answer: 25164150
  */
  def Six(): AnyVal = {
    val nums: List[Long] = List.range(1, 101)
    val sum = nums.sum
    (sum*sum) - (nums.map {x => x*x}.foldLeft(0L) ( _+_ ))
  }


  /*
    What is the 10 001st prime number?
    Answer: 104743
  */
  def Seven(): AnyVal = {
    Sieve.take(10001).last
  }


  /*
    Find the thirteen adjacent digits in the 1000-digit number that have the
    greatest product. What is the value of this product?
    Answer: 23514624000
  */
  def Eight(): AnyVal = {
    val nums = io.Source.fromFile("eight.txt").mkString.toList.map(_.toLong-48L)

    def adjacentProduct(xs: List[Long], depth: Int): List[Long] = {
      if (depth <= 0) xs
      else xs.grouped(13).map {x => x.foldLeft(1L)( _*_ )}.max :: 
        adjacentProduct(xs.tail, depth-1)
    }
    adjacentProduct(nums, 13).max
  }


  /*
    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
    Answer: 31875000
  */
  def Nine(): AnyVal = {
      ( for(a <- (1L to 1000L);
            b <- (1L to 1000L) if ( math.sqrt(a*a + b*b)+a+b == 1000)) yield
          a*b*math.sqrt(a*a + b*b)).head.toLong
  }


  /*
    Find the sum of all the primes below two million.
    Answer: 142913828922
  */
  def Ten(): AnyVal = {
    Sieve.map(_.toLong).takeWhile( _ < 2000000).sum
  }
}