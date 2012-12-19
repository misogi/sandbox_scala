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

  // 24.4
  def ite = {
    val buf = new scala.collection.mutable.ListBuffer[Any]
    val it = Iterable(1,2,3,4,5)
    val xt = Iterable("a", "b", "c", "d", "e", "f")

    val ite = it grouped 3
    buf += ite.next()
    buf += ite.next()
    val sit = it sliding 3
    buf += sit.next()
    buf += sit.next()

    buf += it takeRight 2
    buf += it dropRight 2

    buf += it zip xt
    buf += it zipAll (xt, 8, "g")
    buf += it.zipWithIndex

    buf += it sameElements xt

    buf.toList
  }

  // 24.5
  def seq = {
    val buf = new scala.collection.mutable.ListBuffer[Any]
    val xs = Seq(6,9,7,11,10)
    val ys = Seq(21,25,23)

    buf += xs(2)
    buf += xs isDefinedAt 5
    buf += xs.length
    buf += xs lengthCompare 4
    buf += xs.indices

    buf += xs indexOf 11
    buf += xs lastIndexOf 7
    buf += xs indexOfSlice ys
    buf += xs lastIndexOfSlice ys
    buf += xs indexWhere (_ > 8)
    buf += xs segmentLength (_ > 9, 3)
    buf += xs prefixLength (_ < 10)

    buf += 3 +: xs
    buf += xs :+ 1

    buf += xs padTo(9, 30)

    buf += xs patch(1,ys,3)
    buf += xs updated (3, 33)
    // xs(3) = 34
    //
    buf += xs.sorted
    buf += xs sortWith (_<_)
    buf += xs sortBy (x => x % 9)

    buf += xs.reverse
    buf += xs.reverseIterator
    buf += xs reverseMap (x => x % 9)

    buf += xs startsWith ys
    buf += xs endsWith ys

    buf += xs contains 7
    buf += xs containsSlice ys
    buf += (xs corresponds ys)(_>_)

    val as = Seq(1,1,2,3,4)
    val bs = Seq(3,4,5)
    buf += as intersect bs
    buf += as diff bs
    buf += as union bs
    buf += as.distinct

    buf.toList
  }
}