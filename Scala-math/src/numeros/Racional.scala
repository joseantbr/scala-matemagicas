package numeros

import general.Numericos._
import general.Implicitos._

/**
  * Clase que trabaja con números racionales.
  *
  * @constructor p y q con r = p/q, y un skip que determina si r necesita simplificación.
  * @constructor p y q con r = p/q. Por defecto, intenta simplificar la fracción.
  * @constructor p con r = p/1. No simplifica.
  *
  * Trabaja con la forma reducida de la fracción, y el signo siempre lo lleva el numerador. Para los casos en los
  * que la fracción representa un número entero, el denominador es 1.
  *
  * Parámetros consultables: num (numerador de la fracción simplificada), den (denominador de la fracción simplificada).
  */
class Racional(p : Int, q : Int, skip : Boolean) {
  require(q != 0, "ERROR: denominador igual a 0.")

  def this(p : Int,q : Int) = this(p,q,false)
  def this(p : Int) = this(p,1,true)

  private val d : Int = if(skip) 1 else mcd(p,q)
  private val (nulo, entero) : (Boolean, Boolean) = (p == 0, q == d)
  val (num, den) : (Int, Int) = if(skip) (p,q) else (signo(q)*p/d, signo(q)*q/d)

  def esEntero : Boolean = entero
  def esNulo : Boolean = nulo

  /** Devuelve el racional por pantalla. Si es entero, no escribe el denominador. */
  override def toString : String = {
    if(nulo)
      "0"
    else if(entero)
      "%d".format(num)
    else
      "%d/%d".format(num,den)
  }
  override def equals(obj: Any): Boolean = obj match {
    case obj : Racional => num == obj.num && den == obj.den
    case obj : Int => num == obj && den == 1
    case _ => false
  }
  override def hashCode(): Int = num.hashCode() + den.hashCode()

  def +(r : Racional) : Racional = new Racional(num*r.den + r.num*den, den*r.den)
  def +(r : Int) : Racional = new Racional(num+r*den,den,true)
  def -(r : Racional) : Racional = new Racional(num*r.den - r.num*den, den*r.den)
  def -(r : Int) : Racional = new Racional(num-r*den,den,true)
  def *(r : Racional) : Racional = new Racional(num*r.num, den*r.den)
  def *(r : Int) : Racional = new Racional(num*r,den)
  def /(r : Racional) : Racional = new Racional(num*r.den , den*r.num)
  def /(r : Int) : Racional = new Racional(num , den*r)

  def **(k : Int) : Racional = {
    if(k == 0)
      new Racional(1)
    else if(k > 0)
      new Racional(num**k,den**k)
    else
      new Racional(den**k,num**k)
  }
}

object Racional {
  def apply(p: Int, q: Int, skip: Boolean): Racional = new Racional(p, q, skip)
  def apply(p: Int, q: Int): Racional = new Racional(p, q)
  def apply(p: Int): Racional = new Racional(p)
}