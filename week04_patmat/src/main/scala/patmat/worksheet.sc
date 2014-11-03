package patmat

object worksheet {
  val testCase = List(List(1, 2, 3), 1, 3, List(1, 2))
                                                  //> testCase  : List[Any] = List(List(1, 2, 3), 1, 3, List(1, 2))

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List(y :: ys) :: zs => flatten(y :: ys) :: flatten(zs)
    case y :: ys => y :: flatten(ys)
    case Nil => Nil

  }                                               //> flatten: (xs: List[Any])List[Any]
  flatten(testCase)                               //> res0: List[Any] = List(List(1, 2, 3), 1, 3, List(1, 2))
}