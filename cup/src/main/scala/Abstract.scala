// chapter 20 abstract
trait Abstract {
  type T
  def transform(x: T): T
  val initial: T
}

class Concrete extends Abstract {
  type T = String
  def transform(x: String) = x + x
  val initial = "hi"
}

trait RationalTrait {
  val numerArg: Int
  val denomArg: Int
  lazy val g = {
    require(denomArg != 0)
    gcd(numerArg, denomArg)
  }

  override def toString = numerArg + "/" + denomArg

  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a%b)
}

object Demo {
  lazy val x = {println("initializing x"); "done"}
}

class Food
abstract class Animal {
  type SuitableFood <: Food
  def eat(food: SuitableFood)
}

class Grass extends Food
class Fish extends Food
class Cow extends Animal {
  type SuitableFood = Grass
  //  c.eat(new c.SuitableFood) // OK
  //  c.eat(new Grass) // OK
  //  c.eat(new Food ) //NG
  override def eat(food: Grass) {}
}

class Pasture {
  var animals: List[Animal {type SuitableFood = Grass}] = Nil
}

object Color extends Enumeration {
  val Red, Green , Blue = Value
}