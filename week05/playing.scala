package dani

Object Week5 {


  def flatten(xs:List[Any]): List[Any] = xs match {
    case List() => List()
    case List(zs) :: ys => flatten(zs) ::: flatten(ys)
    case y :: ys => y ++ flatten(ys)

  }



  def squareList(xs: List[Int]):List[Int] = xs match {
    case Nil   => Nil
    case y::ys => y*y :: squareList(ys)
  }

  def squareList2(xs: List[Int]):List[Int] = xs.map(x=>x*x)
    

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 => List(x :: xs1.takeWhile(_==x), xs1.dropWhile(_==x))
  }

  def encode[T](xs: List[T]): List[(T, Int)] = {
    case Nil => Nil
    case x :: xs1 => 
      val (first, rest) = xs.span(_==x)
      (x, first.length) :: encode(rest)
  }

  def mapFun[T, U](xs: List[T], f: T=> U): List[U] = 
    (xs foldRight List[U]()) (??? )

  def lengthFun[T](xs: List[T]) : Int = 
    (xs foldRight 0) (_+1)

  def isPrime(n: Int): Boolean = (2 until n).forall(n%_!=0)

  def primeCombinations(i: Int, j: Int): List[(Int, Int)] = {
    (1 to i).flatMap{x => (1 to j).map{y => (x, y)}}.filter(isPrime(_+_)
  }

}
