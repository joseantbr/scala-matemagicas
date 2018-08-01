package general

import scala.collection.mutable
import general.Implicitos.ModInt

/***
  * Aquí van cosas genéricas.
  */
object General {

  // NUMÉRICAS

  val eps : Double = 1e-7
  val listaprimos : List[Int] = List(2, 3,      5,      7,     11,     13,     17,     19,     23,     29,
  31,     37,     41,     43,     47,     53,     59,     61,     67,     71,
  73,     79,     83,     89,     97,    101,    103,    107,    109,    113,
  127,    131,    137,    139,    149,    151,    157,    163,    167,    173,
  179,    181,    191,    193,    197,    199,    211,    223,    227,    229,
  233,    239,    241,    251,    257,    263,    269,    271,    277,    281,
  283,    293,    307,    311,    313,    317,    331,    337,    347,    349,
  353,    359,    367,    373,    379,    383,    389,    397,    401,    409,
  419,    421,    431,    433,    439,    443,    449,    457,    461,    463,
  467,    479,    487,    491,    499,    503,
  )

  def signo(x : Int) : Int = if(x > 0) 1 else if(x < 0) -1 else 0
  def mcd(x : Int, y : Int) : Int = {
    if(x < 0 || y < 0) mcd(x.abs,y.abs)
    else {
      val r = y
      if (x % y == 0) r else mcd(y, x % y)
    }
  }
  def MCM(x : Int, y : Int) : Int = (x * y).abs / mcd(x,y)

  def aprox[A,B](x : A, y : B)(implicit numA : Numeric[A], numB : Numeric[B]) : Boolean = (numA.toDouble(x) - numB.toDouble(y)).abs < eps

  def factorizar(x : Int) : List[(Int,Int)] = {
    var fact = List[(Int,Int)]()
    var n = x

    if(n == 0) return List((0,1))
    if(n == 1) return List((1,1))

    for(primo <- listaprimos) {
      var i = 0
      while(n % primo == 0) {
        n /= primo
        i += 1
      }

      if(i != 0) fact = (primo, i) :: fact
      if(n == 1) return fact
    }

    if(n != 1) sys.error("ERROR: no se pudo factorizar el número")
    else fact
  }
  def multiplicar(xs : List[(Int,Int)]) : Int = {
    var n = 1
    for((primo, indice) <- xs)
      n *= primo**indice

    n
  }
  def factoresOrdenN(xs : List[(Int,Int)], i : Int) : List[List[(Int,Int)]] = filterp[(Int,Int)](x => x._2 >= i, xs)

  // TRATAMIENTO DE DATOS

  def filterp[A](p : A => Boolean, xs : List[A]) : List[List[A]] = {
    var as = List[A]()
    var bs = List[A]()
    for(x <- xs)
      if(p(x)) as = x :: as else bs = x :: bs

    List(as, bs)
  }
  def sonPerms[A](xs : Array[A], ys : Array[A]) : Boolean = {
    val f = mutable.HashMap[A,Int]()

    for(x <- xs)
      if(f.isDefinedAt(x)) f(x) += 1 else f(x) = 1

    for(y <- ys)
      if(f.isDefinedAt(y)) if(f(y) == 0) return false else f(y) -= 1 else return false

    for(n <- f.values)
      if(n != 0) return false

    true
  }

}
