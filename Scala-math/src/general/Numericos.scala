package general

/***
  * Aquí van funciones numéricas.
  */
object Numericos {
  val eps : Double = 1e-7

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
}
