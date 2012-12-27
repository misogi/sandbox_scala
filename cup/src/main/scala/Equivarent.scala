class Point(var x: Int, val y: Int) {
  override def hashCode = 41 * (41 + x) + y
  override def equals(other: Any): Boolean = other match {
    case that: Point => this.x == that.x && this.y == that.y &&
      this.getClass == that.getClass
    case _ => false
  }
}

object Colord extends Enumeration {
  val Red, Orange, Yellow, Green, Blue, Indigo, Violet = Value
}

class ColoredPoint(x: Int, y: Int, val color: Colord.Value)
  extends Point(x, y) {
  override def equals(other: Any) = other match {
    case that: ColoredPoint => this.color == that.color && super.equals(that)
    case _ => false
  }
}