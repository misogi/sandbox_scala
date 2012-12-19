object LessonCollection{
  def tra = {
    val buf = new scala.collection.mutable.ListBuffer[Any]
    val xs = Traversable(1,2,3,4)
    val ys = Traversable(4,5,6,7)
    def pla(x: Int): Int = x + 1
    buf += xs ++ ys

    buf += xs map pla
    def gt(x: Int) = List(x + 2)
    buf += xs flatMap gt

    val pf: PartialFunction[Int, String] = {case 2 => "two"}
    buf += xs collect pf

    buf.toList
  }
}