object Twenty {
  val usage = """
    Usage: Twenty <11-20>
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


  /*
    Parses the command line arguments to call the right functions.
  */
  def ArgMatch(arg: String): Any = arg match {
    case "11" => Eleven
    case "12" => "Twelve"
    case "13" => "Thirteen"
    case "14" => "Fourteen"
    case "15" => "Fifteen"
    case "16" => "Sixteen"
    case "17" => "Seventeen"
    case "18" => "Eighteen"
    case "19" => "Nineteen"
    case "20" => "Twenty"
    case _ => usage    
  }


  def Eleven(): AnyVal = {
    val grid = (for (line <- io.Source.fromFile("eleven.txt").getLines())
      yield line.split(" ").map( _.toInt).toList).toList

    def vertTranspose(grd: List[List[Int]]): List[List[Int]] = {
      grd match {
        case List(List(), _*) => grd.filter { _ != List() }
        case List(List(_*), _*) => grd.map(_.head) :: vertTranspose(grd.map(_.tail))
        case _ => grd.filter { _ != List() }  // Catchall case
      }
    }
    // def Horiz(grd: Array[Array[Int]]): Int = {
    //   grd.
    // }
    grid.foreach(println)
    println("")
    // vertTranspose(grid).filter { _ != List() }.foreach(println)
    vertTranspose(grid).foreach(println)
    1
  }
}