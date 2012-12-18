// chapter 17 collection
//

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.immutable.TreeSet

object Collections {
  def arrays {
    val output = new ListBuffer[Any]()
    val fiveInts = new Array[Int](5)
    val fiveToOne = Array(5,4,3,2,1)

    // array buffer
    val arrbuf = new ArrayBuffer[Int]()
    arrbuf += 12
    arrbuf += 15

    // set
    val mutaSet = mutable.Set(1,2,3)
    val words = mutable.Set.empty[String]
    val wordsArray = "See Spot run. Run, Spot. Run!".split("[ !,.]+")
    for (word <- wordsArray) words += word.toLowerCase

    // set
    val nums = Set(1,2,3)
    val nums5 = nums + 5
    val nums56 = nums ++ List(5,6)
    val nums13 = nums & Set(1,3,5,7)
    val is3 = nums.contains(3)

    // map
    val map = mutable.Map.empty[String, Int]
    map("hello") = 1
    map("there") = 2
    val mapVi = map + ("vi" -> 6)
    val mapHe = map - "hello"
    val mapiii = map ++ List("iii" -> 3, "v" -> 5)

    val counts = countWords("See Spot run. Run, Spot. Run!")

    // tree
    val ts = TreeSet(9,3,1,8,0,2,7,4,6,5)
    val colors = List("blue", "yellow", "red", "green")
    val ts2 = TreeSet[String]() ++ colors

    //tuple
    val longest = longestWord("The quick brown fox".split(" "))
    val word, index = longest

    output += fiveInts
    output += fiveToOne
    output += arrbuf.toArray
    output += words
    output += (nums, nums5, nums56, nums13)
    output += map
    output += counts
    output += (mapVi, mapHe, mapiii, map.keys, map.keySet, map.values)
    output += (ts, ts2)
    output += (longest._1, longest._2)

    output.toList.map(prints(_))
  }

  def countWords(text: String) = {
    val counts = mutable.Map.empty[String, Int]
    for (rawWord <- text.split("[ ,.!]+")) {
      val word = rawWord.toLowerCase
      val oldCount = if (counts.contains(word)) counts(word) else 0
      counts += (word -> (oldCount + 1))
    }
    counts
  }

  def longestWord(words: Array[String]) = {
    var word = words(0)
    var idx = 0
    for (i <- 0 until words.length)
      if (words(i).length > word.length) {
        word = words(i)
        idx = i
      }
    (word, idx)
  }

  def prints(x: Any) = x match {
    case aa: Array[Int] => aa.foreach(print)
    case ss: Set[_] => println(ss.toString)
    case ms: Map[_, _] => println(ms.toString)
    case _ => println(x)
  }
}