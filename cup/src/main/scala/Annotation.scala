// chapter 27
object Annotation {
  @deprecated("use newShinyMethod as of 0.9.5", "hoge") def bigMistake = println("oops")

  @deprecated("deps", "hoge") class QuickAndDirty

  def annotTest = {
    import annotation._
    class strategy(arg: Annotation) extends Annotation
    class delayed extends Annotation
    @strategy(new delayed) def f(){}
    f
  }

  import scala.annotation.tailrec
  @tailrec
  def f(n: Int, s:Int = 0): Int = {
    if (n <= 0) s else f(n-1, s+n)
  }

  def arrTest(s: List[Int]) = {
    @tailrec def sum(l: List[Int], total: Int): Int =
      if (l == List.empty) total
      else if (l.head == 0) total
      else sum(l.tail, total+l.head)
    sum(s, 0)
  }

  // @unchecked
  // @native
  // @seriarizable
}