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