package launch {
  class Booster3
}

package bobsrockets {
  package navigation {
    package launch {
      class Booster1
    }

    class MissionControl {
      val b1 = new launch.Booster1
      val b2 = new bobsrockets.launch.Booster2
    }

  }

  package launch {
    class Booster2
  }
}

package bobsdelights {

  abstract class Fruit(
    val name: String,
    val color: String
  )

  object Fruits {
    object Apple extends Fruit("Apple", "red")
    object Orange extends Fruit("orange", "orange")
    object Pear extends Fruit("Pear", "yellowish")
    val menu = List(Apple, Orange, Pear)
  }

  class People {
    import Fruits.{Apple => McIntosh, Orange}
    val n = McIntosh.name

    // all fruits
    import Fruits._

    // import without Pear
    import Fruits.{Pear => _, _}
  }
}