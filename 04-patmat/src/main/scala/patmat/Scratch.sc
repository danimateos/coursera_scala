// Assignment

val x = 'a' :: 'b' :: 'a' :: 'b' :: 'c' :: 'c' :: 'c' :: 'b' :: 'c' :: Nil

val y = x.groupBy(x=>x)

val z = y.transform((_, v) => v.length).toList

def makeOrderedLeafList(freqs: List[(Char, Int)]): List[(Char, Int)] =
  freqs.sortBy( _._2)

val omega = makeOrderedLeafList(z)




{ // Video 4.7
  def isort(xs : List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => insert(y, isort(ys))
  }

  def insert(n: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => n :: Nil
    case y :: ys => if (n < y) n :: xs else y :: insert(n, ys)
  }

  isort(5 :: 2 :: 3 :: 6 :: 7 :: 1 :: Nil)
}


{ // Videos 4.5, 4.6
  abstract class Expr


  case class Num(n: Int) extends Expr
  case class Var(x: String) extends Expr
  case class Sum(a: Expr, b: Expr) extends Expr
  case class Prod(a: Expr, b: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Num(n) => n
    case Sum(a, b) => eval(a) + eval(b)
    case _ => throw new NotImplementedError("Fuck variables")
  }

  def show(e: Expr): String = e match {
    case Num(n) => n.toString
    case Var(x) => x
    case Sum(a, b) => show(a) + " + " + show(b)
    case Prod(a, b) => (a, b) match {
      case (Sum(_, _), Sum(_, _)) => "(" + show(a) + ") * (" + show(b) + ")"
      case (Sum(_, _), _) => "(" + show(a) + ") * " + show(b)
      case (_, Sum(_, _)) => show(a) + " * (" + show(b) + ")"
      case _ => show(a) + " * " + show(b)
    }
  }

  show(Prod(Sum(Num(1), Num(2)), Var("x")))
  show(Sum(Prod(Num(1), Num(2)), Var("x")))
}

// Previous videos

abstract class Nat {
  def isZero: Boolean
  def predecessor : Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def predecessor: Nat = n
  override def successor: Nat = new Succ(this)
  override def + (that: Nat): Nat = predecessor + that.successor
  override def - (that: Nat) = if (that.isZero) this else predecessor - that.predecessor

  override def toString = "Succ(" + n.toString + ")"
}

object Zero extends Nat {
  override def isZero: Boolean = true
  override def predecessor: Nat = throw new NullPointerException
  override def successor: Nat = new Succ(this)
  override def + (that: Nat) = that
  override def - (that: Nat) = throw new NullPointerException

  override def toString = "0"
}



val two = new Succ(new Succ(Zero))
val three = new Succ(new Succ(new Succ(Zero)))

three + two
three - two
