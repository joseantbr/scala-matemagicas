package numeros

import general.Implicitos.ModInt
import general.General._

class Raiz(private var c0 : Real,private var r0 : Real, skip : Boolean) extends Ordered[Raiz] {
  require(r0 >= Real(0), "ERROR: radicando negativo")

  private val indice : Int = 2

  private def factorizarRad() : Unit = {
    def extraerRacional(x : Racional) : Racional = {
      val numF = factorizar(x.num)
      val denF = factorizar(x.den)

      var num = 1
      var den = 1

      for((primo, exp) <- numF) {
        val (e1, e2) = exp %/ indice
        c0 *= primo**e1
        num *= primo**e2
      }
      for((primo, exp) <- denF) {
        val (e1,e2) = exp %/ indice
        c0 /= primo**e1
        den *= primo**e2
      }

      Racional(num,den)
    }
    if(r0.esRacional) {
      val frac = extraerRacional(r0.parteRacional)
      if(frac == Racional(0) || coef == Real(0)) {
        r0 = Real(Racional(0))
        c0 = Real(0)
      }
      else r0 = Real(frac)
    }
    else {
      val frac = r0.sacarFactorComun()
      val x = extraerRacional(frac) // c0 ya multiplicó lo que se podía sacar, x es lo que queda
      r0 = r0*(x/frac)
    }
  }

  if(!skip) factorizarRad()

  val coef : Real = c0.clone()
  val rad : Real = r0.clone()

  private val esNulo : Boolean = coef == Real(0)
  private val esRaiz : Boolean = rad != Real(1) && rad != Real(0)

  override def compare(that: Raiz): Int = this.toDouble.compareTo(that.toDouble)
  def toDouble: Double = coef.toDouble * Math.sqrt(rad.toDouble)

  def +(r : Raiz) : Real = new Real(Racional(0),Array(this,r),false)
  def *(r : Raiz) : Raiz = if(rad == r.rad) new Raiz(coef*r.coef*rad,Real(1),true) else new Raiz(coef*r.coef,rad*r.rad,false)
  def *(r : Racional) : Raiz = if(r == 0) new Raiz(Real(0),Real(0),true) else new Raiz(coef*r,rad,true)
  def *(r : Int) : Raiz = this*Racional(r)

  override def toString: String = {
    var st : String = ""

    if(esNulo) return "0"

    if(coef != 1) {
      if(coef == -1) st += "- " else st += coef.toString
    }

    if(esRaiz)
      if(rad.esSimple)
        if(rad.esRacional) st += '√'+rad.toString
        else if(rad(0).coef == 1) st += '√'+rad.toString
        else st += '√'+"("+rad.toString+")"

    st
  }
  override def equals(obj: Any): Boolean = obj match {
    case obj : Raiz => coef == obj.coef && rad == obj.rad
    case _ => false
  }
  override def hashCode(): Int = coef.hashCode() + 2*rad.hashCode()
  override def clone(): Raiz = new Raiz(coef,rad,true)
}
object Raiz {
  def apply(rad : Real) : Raiz = new Raiz(Real(1),rad,false)
  def apply(rad : Raiz) : Raiz = new Raiz(Real(1),Real(rad),false)
  def apply(rad : Int): Raiz = new Raiz(Real(1),Real(rad),false)
  def apply(rad : Racional) : Raiz = new Raiz(Real(1),Real(rad),false)
}
