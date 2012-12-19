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

  def buf = {
    val p = new scala.collection.mutable.ListBuffer[Any]
    val bf = scala.collection.mutable.Buffer(1)
    bf += 2
    bf += (3,4)
    bf ++= List(5,6)
    7 +=: bf
    List(8,9) ++=: bf
    bf insert (5, 10)
    bf insertAll (8, List(11,12))

    val bf1 = scala.collection.mutable.Buffer(1,2,3,4,5,6,7,8,9,10,11,12)
    bf1 -= 1
    bf1 remove 2
    bf1 remove (3,3)
    bf1 trimStart 2
    bf1 trimEnd 2
    // bf1.clear

    List(bf, bf1)
  }

  // 24.6 集合
  def set = {
    val p = new scala.collection.mutable.ListBuffer[Any]
    val xs = Set("apple", "orange", "peach", "banana")
    val ys = Set("apple", "peach", "meron")
    p += xs contains "apple"
    p += xs("orange")
    p += ys subsetOf xs
    p += xs + "cherry"
    p += xs + ("watermeron", "meron")
    p += xs ++ ys
    p += xs - "orange"
    p += xs -- ys
    p += xs.empty

    //same
    p += xs & ys
    p += xs intersect ys

    //same
    p += xs | ys
    p += xs union ys

    p += xs &~ ys
    p += xs diff ys

    // ordered numbers
    val myOrdering = Ordering.fromLessThan[String](_ > _)
    val ts = scala.collection.immutable.TreeSet.empty(myOrdering)
    p += ts
    val nums = ts + ("one", "two", "three", "four")
    p += nums
    p += nums range ("two", "one")
    p += nums from "three"

    p.toList
  }

  // 24.7 写像
  def map = {
    val p = new scala.collection.mutable.ListBuffer[Any]
    // immutable
    val ms = Map("cat" -> 1, "x" -> 3, "y" -> 5)
    p += ms get "cat"
    p += ms("x")
    p += ms getOrElse("t", 30)
    p += ms contains "x"
    p += ms isDefinedAt "y"
    p += ms + ("dog" -> 4, "t" -> 6)
    val ks = Map("bird" -> 8)
    p += ms ++ ks
    p += ms updated ("penguin", 9)
    p += ms - "cat"
    p += ms - ("x", "y")
    p += ms -- List("cat", "x")
    p += ms.keys
    p += ms.keySet
    p += ms.keysIterator
    p += ms.values
    p += ms.valuesIterator

    p += ms filterKeys (_ == "x")
    p += ms mapValues (x => x * x)

    // mutable
    val mms = scala.collection.mutable.Map("a"->3, "b"->10, "c"->7)
    mms("b") = 22
    println(mms)
    mms += ("cat" -> 45)
    println(mms)
    mms put ("dog", 52)
    println(mms)
    mms getOrElseUpdate("penguin", 69)
    println(mms)
    mms -= "b"
    println(mms)
    mms --= List("a", "c")
    println(mms)
    mms retain ((x,y) => x == y)
    println(mms)
    mms += ("cat" -> 45)
    mms transform ((x: String,y: Int) => y * y)
    println(mms)

    p.toList
  }

  // 24.8 synchronized
  def sync = {
    // thread safe map
    import scala.collection.mutable.{Map, SynchronizedMap, HashMap}
    def makeMap: Map[String, String] = {
      new HashMap[String, String] with SynchronizedMap[String, String] {
        override def default(key: String) = "Why do you want to know?"
      }
    }
    val capital = makeMap
    capital ++= List("US" -> "washington")
  }

  // 24.9 具象イミュータブルコレクション
  def imc = {
    val p = new scala.collection.mutable.ListBuffer[Any]
    val str = 1 #:: 2 #:: 3 #:: Stream.empty
    p += str
    def fibFrom(a: Int, b: Int): Stream[Int] = a #:: fibFrom(b, a + b)
    val fibs = fibFrom(1,1).take(7)
    p += fibs.toList

    // vector
    val vec = scala.collection.immutable.Vector.empty
    val vec2 = vec :+ 1 :+ 2
    val vec3 = 100 +: vec2
    p += (vec, vec2, vec3)

    // stack
    val stk = scala.collection.immutable.Stack.empty
    val hasOne = stk.push(1)
    p += hasOne.top

    // queue
    val q = scala.collection.immutable.Queue[Int]()
    val q1 = q.enqueue(1)
    val q2 = q1.enqueue(List(2,3))
    val (el, q3) = q2.dequeue
    p += (q,q1,q2,el,q3)

    //range
    p += 1 to 3
    p += 5 to 14 by 3
    p += 1 until 4

    // bitset
    val bits = scala.collection.immutable.BitSet.empty
    val bt = bits + 3 + 4 + 4
    p += bt(4)
    p += bt(0)

    p.toList
  }

  // 24.10 具象ミュータブルコレクション
  def mc = {
    // array
    val ab = collection.mutable.ArrayBuffer.empty[Int]
    ab += 1
    println(ab)
    println(ab.toArray)

    // list
    // string
    val sb = new StringBuilder
    sb += 'a'
    println(sb)
    sb ++= "bcdef"
    println(sb)
    println(sb.toString)

    val q = new scala.collection.mutable.Queue[String]
    q += "a"
    println(q)
    q ++= List("b", "c")
    println(q)
    println(q.dequeue)
    println(q)

    // stack
    val st = new scala.collection.mutable.Stack[Int]
    st.push(3)
    println(st)
    st.push(2)
    println(st)
    println(st.top)
    println(st)
    println(st.pop)
    println(st)

    // hash map
    val map = collection.mutable.HashMap.empty[Int, String]
    map += (1 -> "make web")
    println(map)
  }
}