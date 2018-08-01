package general

import numeros.{Racional, Raiz, Real}

object Implicitos {

  implicit class ModInt(a: Int) {
    def +(b: Racional): Racional = new Racional(a * b.den + b.num, b.den, true)
    def +(b : Raiz) : Real = b+Raiz(a)
    def +(b : Real) : Real = b+a
    def -(b: Racional): Racional = new Racional(a * b.den - b.num, b.den, true)
    def -(b : Raiz) : Real = Raiz(a)*(-1) + Raiz(a)
    def -(b : Real) : Real = b*(-1) + a
    def *(b: Racional): Racional = new Racional(a * b.num, b.den)
    def *(b : Raiz) : Raiz = b*a
    def *(b : Real) : Real = b*a
    def /(b: Racional): Racional = new Racional(a * b.den, b.num)
    def /(b : Raiz) : Real = Real(a)/Real(b)
    def /(b : Real) : Real = Real(a)/b

    def %/(b: Int): (Int, Int) = {
      var n = 0
      var r = a
      while (r - b >= 0) {
        n += 1
        r -= b
      }
      (n, r)
    }

    def **(b: Int): Int = {
      var x = 1
      for (_ <- 1 to b)
        x *= a

      x
    }
  }

  implicit class ModBoolean(a: Boolean) {
    def xor(b: Boolean): Boolean = (a && !b) || (!a && b)
  }

  /*
  implicit object RacionalNumerico extends Fractional[Racional] {
    override def plus(x: Racional, y: Racional): Racional = x+y
    override def minus(x: Racional, y: Racional): Racional = x-y
    override def times(x: Racional, y: Racional): Racional = x*y
    override def div(x: Racional, y: Racional): Racional = x/y
    override def negate(x: Racional): Racional = new Racional(-x.num,x.den,true)
    override def fromInt(x: Int): Racional = new Racional(x)
    override def toInt(x: Racional): Int = x.num / x.den
    override def toLong(x: Racional): Long = x.num / x.den
    override def toFloat(x: Racional): Float = x.num / x.den.toFloat
    override def toDouble(x: Racional): Double = x.num / x.den.toDouble
    override def compare(x: Racional, y: Racional): Int = x.compare(y)

    override def one: Racional = new Racional(1)
    override def zero: Racional = new Racional(0)
  }*/

}
