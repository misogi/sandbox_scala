// chapter 23
object LessonFor {
  case class Person(name: String, isMale: Boolean, children: Person*)
  def listPersons = {
    val lara = Person("Lara", false)
    val bob  = Person("Bob", false)
    val julie = Person("Julie", false , lara, bob)
    val persons = List(lara, bob, julie)
    val l = for (p <- persons; n = p.name; if (n startsWith "Bo")) yield n
    val l2 = for (x <- List(1,2); y <- List("one", "two")) yield (x, y)
    println(l)
    println(l2)
  }

  //23.2
  def queens(n: Int): List[List[(Int, Int)]] = {
    def placeQueens(k: Int): List[List[(Int, Int)]] =
      if (k == 0)
        List(List())
      else
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafe(queen, queens)
        } yield queen :: queens
    placeQueens(n)
  }

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = queens forall (q => !inCheck(queen, q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)) =
    q1._1 == q2._1 || q1._2 == q2._2 || (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

  // 23.3
  case class Book(title: String, authors: String*)

  val books: List[Book] =
    List(
      Book(
        "stracture",
        "Abelson", "Sussman"
      ),
      Book(
        "Priciples",
        "Aho", "Sussman"
      ),
      Book(
        "Java",
        "Gosling", "Joy", "Steele"
      )
    )

  def findBooks = for (b <- books; a <- b.authors
    if a startsWith "Gosling") yield b.title

  def findByTitle = for (b <- books
    if (b.title indexOf "rincip") >= 0) yield b.title

  def findTwoBooks  = for (b1 <- books; b2 <- books if b1 != b2;
    a1 <- b1.authors; a2 <- b2.authors if a1 == a2) yield a1

  def removeDuplicates[A](xs: List[A]): List[A] = {
    if (xs.isEmpty) xs
    else
      xs.head :: removeDuplicates(
        xs.tail filter (x => x != xs.head)
      )
  }

  // 23.4
  def exprConvert = {
    val ls = List(1,4,3,2,5,6,7,8,2,3,9)

    val ls41a = for (x <- ls) yield x+1
    val ls41b = ls.map(x => x+1)

    println((ls41a, ls41b))

    val ls42a = for (x <- ls if x > 5) yield x+2
    val ls42b = for (x <- ls withFilter (x => x > 5)) yield x+2
    val ls42c = ls withFilter (x => x > 5) map (x => x+2)

    println((ls42a, ls42b, ls42c))

    val ls2 = List("a", "c", "b", "x")
    val ls43a = for (x <- ls; y <- ls2; if x > 6) yield y + x
    val ls43b = ls.flatMap(x => for (y <- ls2; if x > 6) yield y + x)
    println((ls43a, ls43b))

    // same result
    val ls45a = for (x <- ls; y = x * 2; if x > 2) yield x + 4
    val ls45b = for ((x, y) <- for(x <- ls) yield (x, x * 2); if x > 2) yield x + 4
    println((ls45a, ls45b))
  }
}