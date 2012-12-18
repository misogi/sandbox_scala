// chapter 21 last
object Mocha extends App {
  class PreferredDrink(val preference: String)
  implicit val pref = new PreferredDrink("mocha")
  def enjoy(name: String)(implicit drink: PreferredDrink) {
    print("welcome. "+name)
    print("enjoy a")
    print(drink.preference)
    println("!")
  }
  enjoy("reader")
}