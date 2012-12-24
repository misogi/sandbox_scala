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

object Matcher {
  def isMatch(s: String) = s match {
    case Twice(s) => "it's twice! " + s + s
    case Email(u,d) => "it's email! " + u + d
  }
}