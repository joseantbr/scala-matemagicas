package general

import numeros.Racional

object Implicitos {
  implicit class ModInt(a : Int) {
    def +(b : Racional) : Racional = new Racional(a*b.den+b.num,b.den,true)
    def -(b : Racional) : Racional = new Racional(a*b.den-b.num,b.den,true)
    def *(b : Racional) : Racional = new Racional(a*b.num,b.den)
    def /(b : Racional) : Racional = new Racional(a*b.den,b.num)

    def **(b : Int): Int = {
      var x = 1
      for(i <- 1 to b)
        x *= a

      x
    }
  }
}
