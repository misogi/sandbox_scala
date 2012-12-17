// chapter 16 List
class FruitList

object FruitList {
  def listTest = {
    val fruit: List[String] = List("apples", "oranges", "pears")
    val nums = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    val diag3 = List(
      List(1,0,0),
      List(0,1,0),
      List(0,0,1)
    )
    val empty = List()

    for(l <- Array(fruit, nums, diag3, empty, fruit.head, fruit.tail.head)) println(l)

    val List(a, b, c) = fruit

    val b1 :: b2 :: rest = fruit
    for(l <- Array(a, b, c, b1, b2, rest)) println(l)

    val abcde = List("a", "b", "c", "d", "e")
    val abt = abcde take 2
    val abd = abcde drop 2
    val abs = abcde splitAt 2
    val abz = abcde.indices zip abcde
    val abuz = abz.unzip
    for(l <- Array(abt, abd, abs, abz, abuz)) println(l)

    val buffer = scala.collection.mutable.ListBuffer[List[Any]]()
    val words = List("the", "quick", "brown", "fox")

    buffer += words filter (_.length == 3)
    val (list1, list2) = nums partition (_ % 2 == 0)
    buffer += list1
    buffer += list2
    buffer += List.range(1,5)
    buffer += List.range(1, 9, 2)
    buffer += List.fill(5)('a')
    buffer += List.tabulate(5)(n => n* n)
    buffer += (List(10,20), List(3,4,5)).zipped.map(_ * _)

    def sum(xs: List[Int]): Int = (0 /: xs)(_ + _)

    val goukei = sum(List(3,4,5))
    println(goukei)

    buffer.toList.map(println(_))
  }

  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case x :: xs1 => insert(x, isort(xs1))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List()  => List(x)
    case y :: ys => if(x <= y) x :: xs
      else y :: insert (x, ys)
  }

  def append[T](xs: List[T], ys: List[T]): List[T] =
    xs match {
      case List()   => ys
      case x :: xs1 => x :: append(xs1, ys)
    }
}