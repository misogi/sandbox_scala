package Fruits
abstract class Fruit
class Apple extends Fruit
class Orange extends Fruit

import scala.collection.mutable.ListBuffer

object Fruits{
  def incAll(xs: List[Int]): List[Int] = {
    val buf = new ListBuffer[Int]
    for (x <- xs) buf += x + 1
    buf.toList
  }
}