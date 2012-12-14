class Rational(n: Int, d: Int) extends Ordered[Rational] {
  val numer = n
  val denom = d
  def compare(that: Rational) = (this.numer * that.denom) - (that.numer * this.denom)
}