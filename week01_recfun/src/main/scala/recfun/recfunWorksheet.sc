package recfun

object recfunWorksheet {

  def reverse(lista: List[Int]): List[Int] = lista match {
    case x :: Nil => lista
    case x :: xs => reverse(xs) ++ List(x)
  }                                               //> reverse: (lista: List[Int])List[Int]
  


}