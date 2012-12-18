// chapter 21
package MyConverter

// import MyConverter.Converter._
object Converter {
  implicit def intToRational(x: Int) = new Rational(x, 1)

  def maxListImpParm[T](elements: List[T])(implicit orderer: T => Ordered[T]): T =
    elements match {
      case List() => throw new IllegalArgumentException("empty!")
      case List(x) => x
      case x :: rest =>
        val maxRest = maxListImpParm(rest)(orderer)
        if (orderer(x) > maxRest) x
        else maxRest
    }

  // Ordered[T] として扱える任意の T を使える
  def maxList[T <% Ordered[T]](elements: List[T]): T =
    elements match {
      case List()    => throw new IllegalArgumentException("empty!")
      case List(x)   => x
      case x :: rest =>
        val maxRest = maxList(rest)
        if (x > maxRest) x
        else maxRest
    }
}

class Rational (n: Int, d: Int){
  val nv = n
  val dv = d
  def + (that: Rational): Rational = new Rational(this.nv + that.nv, this.dv + that.nv)
  override def toString = nv + "/" + dv
}

class PreferredPrompt(val preference: String)
class PreferredDrink(val preference: String)

object Greeter {
  def greet(name: String)(implicit prompt: PreferredPrompt, drink: PreferredDrink) = {
    println("welcome, "+name+". The system is ready")
    println("but while you work.")
    println("why not enjoy a cup of "+drink.preference+"?" )
    println(prompt.preference)
  }
}

// import MyConverter.JoesPrefs._
object JoesPrefs {
  implicit val prompt = new PreferredPrompt("yes, master> ")
  implicit val drank  = new PreferredDrink("sake")
}

