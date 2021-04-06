import scala.annotation.tailrec
import funsets.FunSets

// Exercise in video 2.1
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else {
      loop(a + 1, acc + f(a))
    }
  }

  loop(a, 0)
}

sum(x => x * x)(5, 10)

// Exercise in video 2.2
def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else {
      loop(a + 1, acc * f(a))
    }
  }

  loop(a, 1)
}

product(x => x)(5, 7)

def factorial(x: Int): Int = product(x => x)(1, x)

factorial(4)

def fold(f: Int => Int, nullValue: Int, op: (Int, Int) => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else {
      loop(a + 1, op(acc, f(a)))
    }
  }
  loop(a, nullValue)
}

fold(x => x, 1, (a, b) => a*b)(1,5)
fold(x => x, 0, (a, b) => a+b)(1,5)

// Exercise in video 2.5
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  override def toString = x + "/" + y

  def + (other: Rational) = new Rational(numer * other.denom + denom * other.numer, denom * other.denom)

  def unary_- = new Rational(-numer, denom)
  def - (other: Rational) = this + -other
}

val x = new Rational(1,2)

x - (new Rational(1,3))

// FunSets.toString(FunSets.map( _%10==0, 2*_))

def evens: FunSets.FunSet = _ % 2 == 0

FunSets.forall(evens, evens)