// chapter 26
object Email extends ((String, String) => String) {
  def apply(user: String, domain: String) = user + "@" + domain
  def unapply(str: String): Option[(String, String)] = {
    val parts = str split "@"
    if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

object Twice {
  def apply(s: String): String = s + s
  def unapply(s: String): Option[String] = {
    val length = s.length / 2
    val half = s.substring(0, length)
    if (half == s.substring(length)) Some(half) else None
  }
}

object UpperCase {
  def unapply(s: String): Boolean = s.toUpperCase == s
}

object Domain {
  def apply(parts: String*): String = parts.reverse.mkString(".")
  def unapplySeq(whole: String): Option[Seq[String]] =
    Some(whole.split("\\.").reverse)
}

object Matcher {
  def isMatch(s: String) = s match {
    case Twice(s) => "it's twice! " + s + s
    case Email(u,d) => "it's email! " + u + d
  }

  def userTwiceUpper(s: String) = s match {
    case Email(Twice(x @ UpperCase()), domain) =>
      "match: " + x + " in domain" + domain
    case _ => "no match"
  }

  def isTomInDotCom(s: String): Boolean = s match {
    case Email("tom", Domain("com", _*)) => true
    case _ => false
  }

  def regexTest = {
    val dec = """(-)?(\d+)(\.\d*)?""".r
    val input = "for -1.0 to 99 by 3"
    for (s <- dec findAllIn input)
      println(s)
    println(dec findFirstIn input)
    println(dec findPrefixOf input)

    val dec(sign, integerpart, decimalpart) = "-1.23"
    println(sign)
    println(integerpart)
    println(decimalpart)

    val dec(s2, i2, d2) = "1.0"
    println(s2)
    println(i2)
    println(d2)

    for (dec(s, i, d) <- dec findAllIn "for -1.0 to 99 by 3")
      println("s: "+s+", i: "+i+", d: "+d)
  }
}