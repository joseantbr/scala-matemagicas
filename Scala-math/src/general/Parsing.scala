package general

import java.util.Scanner
import numeros.Racional

/**
  * Objeto que recopila funciones de tratamiento de Strings.
  */
object Parsing {
  def arrayToString[A](xs : Array[A]) : String = xs.mkString("Array(",",",")")

  def racionalFromString(st : String) : Racional = {
    val scanner = new Scanner(st).useDelimiter("[/]")
    val num = scanner.nextInt()
    var den = 1
    if(scanner.hasNextInt())
      den = scanner.nextInt()

    scanner.close()

    new Racional(num,den)
  }
  def racionalToString(r : Racional) : String = r.toString
}
