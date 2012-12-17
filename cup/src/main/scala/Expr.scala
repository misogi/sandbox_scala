package ExprSample

abstract class Expr
case class Var(name: String) extends Expr
case class NumberVar(num: Double) extends Expr
case class UnOp(operator: String, arg: Expr) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

object Expr {


  def simplifyTop(expr: Expr): Expr = expr match {
    case UnOp("-", e @ UnOp("abs", _)) => e
    case UnOp("-", UnOp("-", e)) => e
    // pattern guard
    case BinOp("+", x, y) if x == y => BinOp("*", x, NumberVar(2))
    case BinOp(_, e, _) => e
    case _ => expr
  }

  import math.{E, Pi}

  def describe(x: Any) = x match {
    case 5        => "five"
    case true    => "true!"
    case "hello" => "hi"
    case Pi      => "strange math? Pi = " + Pi
    case Nil     => "empty"
    case _       => "else"
  }

  def listDemo(l: Any) = l match {
    case (a, b, c )   => println("matched " + a + b + c)
    case s: String    => s.length
    case n: Int if 0 < n => "int" + n
    case m: Map[_, _] => m.size
    case _            =>
  }

  def isStringArray(x: Any) = x match {
    case a: Array[String] => "yes"
    case _                => "no"
  }

  def simplifyAll(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplifyAll(e)
    case UnOp(op, e) => UnOp(op, simplifyAll(e))
    case _ => expr
  }

  // show (capitals get "France")
  def show(x: Option[String]) = x match {
    case Some(s) => s
    case None    => "?"
  }

  val withDefault: Option[Int] => Int = {
    case Some(x) => x
    case None    => 0
  }

  val second: PartialFunction[List[Int], Int] = {
    case x :: y :: _ => y
  }

  def forPattern
  {
    val capitals = Map("France"->"Paris", "Japan"->"Tokyo")
    for((country, city) <- capitals) println ("the capital of " + country + " is " + city)
    val fruits = List(Some("apple"), None, Some("orange"))
    for (Some(fruit) <- fruits) println(fruit)
  }
}
