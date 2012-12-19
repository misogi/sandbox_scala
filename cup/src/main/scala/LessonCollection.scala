// chapter 24
object LessonCollection{
    // 24.3
  def tra = {
    val buf = new scala.collection.mutable.ListBuffer[Any]
    val xs = Traversable(1,2,3,4,3)
    val ys = Traversable(4,5,6,7)
    def pla(x: Int): Int = x + 1
    buf += xs ++ ys

    buf += xs map pla
    def gt(x: Int) = List(x + 2)
    buf += xs flatMap gt

    val pf: PartialFunction[Int, String] = {case 2 => "two"}
    buf += xs collect pf

    buf += xs.toArray
    buf += xs.toList
    buf += xs.toIterable
    buf += xs.toStream
    buf += xs.toSet
    // buf += xs.toMap

    val dest = Traversable(2,3,4)
    xs copyToBuffer buf

    buf += xs.isEmpty
    buf += xs.nonEmpty
    buf += xs.size
    buf += xs.hasDefiniteSize

    buf += xs.head
    buf += xs.headOption
    buf += xs.last
    buf += xs.lastOption
    buf += xs find (_ == 2)

    buf += xs.tail
    buf += xs.init
    buf += xs slice (1,2)
    buf += xs take 3
    buf += xs drop 2
    buf += xs takeWhile (_<=2)
    buf += xs dropWhile (_==2)
    buf += xs filter (_>2)
    buf += xs withFilter (_>3)
    buf += xs filterNot (_>2)

    buf += xs splitAt 2
    buf += xs span (_==2)
    buf += xs partition (_>2)
    buf += xs groupBy (_>2)

    buf += xs forall (_==3)
    buf += xs exists (_==3)
    buf += xs count (_==3)

    val z = 0
    buf += (z /: xs)(_+_)
    // similar to behind
    buf += xs.foldLeft(z)(_+_)
    buf += (xs :\ z)(_+_)
    buf += xs.foldRight(z)(_+_)

    buf += xs reduceLeft (_+_)
    buf += xs reduceRight (_*_)

    buf += xs.sum
    buf += xs.product
    buf += xs.min
    buf += xs.max

    val bu = new StringBuilder
    buf += xs addString (bu, "[[", "+", "]]")
    buf += xs mkString ("**(**", "@", "**)**")
    buf += xs.stringPrefix

    buf += xs.view
    buf += xs view (2,3)

    buf.toList
  }
}