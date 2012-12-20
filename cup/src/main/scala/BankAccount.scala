class BankAccount {
  private var bal: Int = 0

  def balance: Int = bal

  def deposit(amount: Int) {
    require (amount > 0)
    bal += amount
  }

  def withdraw(amount: Int): Boolean =
    if (amount > bal) false
    else {
      bal -= amount
      true
    }
}

class Thermometer {
  var celsius: Float = _
  def fahrenheit = celsius * 9 / 5 + 32
  def fahrenheit_= (f: Float) {
    celsius = (f - 32) * 5/ 9
  }
  override def toString = fahrenheit + "F/" + celsius + "C"
}