
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