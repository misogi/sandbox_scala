abstract class CurrencyZone {
  type Currency <: AbstractCurrency
  val CurrencyUnit: Currency
  def make(x: Long): Currency

  abstract class AbstractCurrency {
    val amount: Long

    def designation: String

    override def toString = ((amount.toDouble / CurrencyUnit.amount.toDouble)
      formatted ("%."+ decimals(CurrencyUnit.amount) + "f")
      + " " + designation)


    def + (that: Currency): Currency = make(this.amount + that.amount)

    def * (x: Double): Currency = make((this.amount * x).toLong)

    def from(other: CurrencyZone#AbstractCurrency): Currency =
      make(math.round(
        other.amount.toDouble * Converter.exchangeRate(other.designation)(this.designation)))

    private def decimals(n: Long): Int = if (n == 1) 0 else 1 + decimals(n/10)
  }
}

object US extends CurrencyZone {
  type Currency = Dollar
  val Cent = make(1)
  val Dollar = make(100)
  val CurrencyUnit = Dollar

  abstract class Dollar extends AbstractCurrency {
    def designation = "USD"
  }

  def make(cents: Long) = new Dollar {val amount = cents}
}

object Europe extends CurrencyZone {
  type Currency = Euro
  val Cent = make(1)
  val Euro = make(100)
  val CurrencyUnit = Euro
  abstract class Euro extends AbstractCurrency {
    def designation = "EUR"
  }

  def make(cents: Long) = new Euro {
    val amount = cents
  }
}

object Japan extends CurrencyZone {
  abstract class Yen extends AbstractCurrency {
    def designation = "JPY"
  }
  type Currency = Yen
  def make(yen: Long) = new Yen {
    val amount = yen
  }
  val Yen = make(1)
  val CurrencyUnit = Yen
}

object Converter {
  var exchangeRate = Map(
    "USD" -> Map("USD" -> 1.0,    "EUR" -> 0.7596, "JPY" -> 1.211, "CHF" -> 1.223),
    "EUR" -> Map("USD" -> 1.316,  "EUR" -> 1.0000, "JPY" -> 1.594, "CHF" -> 1.623),
    "JPY" -> Map("USD" -> 0.8257, "EUR" -> 0.6272, "JPY" -> 1.000, "CHF" -> 1.018),
    "CHF" -> Map("USD" -> 0.8108, "EUR" -> 0.6160, "JPY" -> 0.982, "CHF" -> 1.000)
    )
}