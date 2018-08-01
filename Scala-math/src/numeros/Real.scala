package numeros

import general.General._
import general.Implicitos._

import scala.collection.mutable
/**
  * Representa un real de la forma r + [r1,...,rn], donde r es racional y r1,...,rn son raices.
  *
  *
  */
class Real(private var racional : Racional, private var raiz : Array[Raiz], skip : Boolean) extends Ordered[Real] {


  private def simplificarRaices() : Unit = {
    val mapa = mutable.HashMap[Real,Real]()

    for(x <- raiz)
      if(mapa.isDefinedAt(x.rad)) mapa(x.rad) += x.coef else mapa(x.rad) = x.coef

    if(mapa.isDefinedAt(Real(1))) {
      racional += mapa(Real(1)).parteRacional
      for(x <- mapa(Real(1)).raices)
        if(mapa.isDefinedAt(x.rad)) mapa(x.rad) += x.coef else mapa(x.rad) = x.coef
      mapa -= Real(1)
    }

    for(x <- mapa.keys)
      if(mapa(x) == Real(0)) mapa -= x

    val ite = mapa.keysIterator
    val n = mapa.size

    raiz = new Array[Raiz](n)

    for(i <- 0 until n) {
      val x = ite.next()
      raiz(i) = new Raiz(mapa(x),x,true)
    }
  }
  private def factorComun(a : Racional, b : Racional) : Racional = {
    if(a == 0) return b
    if(b == 0) return a
    val signo = if(a*b > Racional(0)) general.General.signo(a.num) else 1

    new Racional(signo*mcd(a.num,b.num),mcd(a.den,b.den),true)
  }
  def sacarFactorComun() : Racional = {
    val fracciones = raices.map(x => x.coef.parteRacional)

    fracciones.foldLeft(parteRacional)((x,y) => factorComun(x,y))
  }

  if(!skip) simplificarRaices()

  val parteRacional : Racional = racional.clone()
  val raices : Array[Raiz] = raiz.clone()

  def this(r : Racional) = this(r,Array[Raiz](),false)
  def apply(n : Int) : Raiz = raices(n)

  val esRacional : Boolean = raices.isEmpty
  val esSimple : Boolean = esRacional xor (parteRacional == Racional.zero && raices.length == 1)

  def +(r : Real) : Real = new Real(parteRacional + r.parteRacional, raices.union(r.raices),false)
  def +(r : Raiz) : Real = new Real(parteRacional, raices.union(Array(r)),false)
  def +(r : Racional) : Real = new Real(parteRacional + r,raices,true)
  def +(r : Int) : Real = this + Racional(r)
  def -(r : Real) : Real = new Real(parteRacional - r.parteRacional, raices.union(r.raices.map(x => x*(-1))),false)
  def -(r : Raiz) : Real = new Real(parteRacional, raices.union(Array(r*(-1))),false)
  def -(r : Racional) : Real = new Real(parteRacional - r,raices, true)
  def -(r : Int) : Real = this - Racional(r)
  def *(r : Real) : Real = {
    val xs = new Array[Raiz]((raices.length+1)*(r.raices.length+1)-1)
    var i = 0
    for(y <- r.raices) {
      xs(i) = y*parteRacional
      i += 1
    }
    for(x <- raices) {
      xs(i) = x*r.parteRacional
      i += 1
      for(y <- r.raices) {
        xs(i) = x*y
        i += 1
      }
    }
    new Real(parteRacional * r.parteRacional,xs,false)
  }
  def *(r : Racional) : Real = new Real(parteRacional*r,raices.map(x => x*r),true)
  def *(r : Int) : Real = this * Racional(r)
  def /(r : Real) : Real = {
    var num = this.clone()
    var den = r.clone()
    while(!den.esRacional) {
      num = num * den.conjugado
      den = den * den.conjugado
      val d = factorComun(num.sacarFactorComun(),den.sacarFactorComun())
      num = num/d
      den = den/d
    }

    num/den.parteRacional
  }
  def /(r : Racional) : Real = this * (r**(-1))
  def /(r : Int) : Real = this * new Racional(1,r,true)

  def **(k : Int) : Real = {
    require(k >= 0, "ERROR: potencia negativa de un real.")
    if(k == 0) Real(1) else this * (this**(k-1))
  }

  def conjugado : Real = new Real(parteRacional,raices.map(x => x*(-1)),true)
  def toDouble : Double = parteRacional.toDouble + raices.map(x => x.toDouble).sum
  override def compare(that: Real): Int = this.toDouble.compare(that.toDouble)

  override def equals(obj: Any): Boolean = obj match {
    case obj : Real => parteRacional == obj.parteRacional && sonPerms(raices,obj.raices)
    case obj : Racional => esRacional && parteRacional == obj
    case obj : Int => esRacional && parteRacional == Racional(obj)
    case obj : Raiz => !esRacional && esSimple && raices(0) == obj
    case _ => false
  }
  override def hashCode(): Int = parteRacional.hashCode() + raices.map(x => x.hashCode()).sum
  override def clone(): Real = new Real(parteRacional,raices,true)
  override def toString: String = {
    var st = ""
    for(x <- raices) {
      val s = x.toString
      if(s(0) != '-') st += "+ "
      st += s+' '
    }

    if(st.isEmpty) parteRacional.toString()
    else if(parteRacional.esNulo)
      if(st(0) == '+') st.drop(2).init
      else st.init
    else parteRacional.toString()+' '+st.init
  }
}
object Real {
  def apply(r: Racional, raices: Array[Raiz], skip : Boolean): Real = new Real(r, raices,skip)
  def apply(raices : Array[Raiz]) : Real = new Real(Racional(0),raices,false)
  def apply(r : Raiz) : Real = new Real(Racional(0),Array(r),true)
  def apply(r : Racional) : Real = new Real(r,Array[Raiz](),true)
  def apply(r : Int) : Real = new Real(Racional(r),Array[Raiz](),true)
}
