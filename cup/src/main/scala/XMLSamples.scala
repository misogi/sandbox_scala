// chapter 28
object XMLSamples {
  def test = {
    val hello = <a> {"hello"+"world"}</a>
    val yearMade = 1955
    val years = <a> {if (yearMade < 2000) <old>{yearMade}</old> else xml.NodeSeq.Empty} </a>
    val secure = <a>{"<b>test</b>"}</a>
    val insecure = "<a>" + "</a>security hole<a>"+"</a>"
    println(hello)
    println(years)
    println(secure)
    println(insecure)
    println(hello.text)
    println(secure.text)

    val hell = <a><b><c>hell!</c></b></a>
    println(hell \ "a")
    println(hell \ "b")
    println(hell \ "c")
    println("-----")
    println(hell \\ "a")
    println(hell \\ "b")
    println(hell \\ "c")

    val joe = <employee name="Joe" />
    println(joe \ "@name")
  }

  abstract class CCTherm {
    val description: String
    val yearMade: Int
    val dateObtained: String
    val bookPrice: Int
    val purchasePrice: Int
    val condition: Int
    override def toString = description
    def toXML =
      <cctherm>
        <description>{description}</description>
        <yearMade>{yearMade}</yearMade>
        <dateObtained>{dateObtained}</dateObtained>
        <bookPrice>{bookPrice}</bookPrice>
        <purchasePrice>{purchasePrice}</purchasePrice>
        <condition>{condition}</condition>
      </cctherm>

    def fromXML(node: scala.xml.Node): CCTherm = new CCTherm {
      val description = (node \ "description").text
      val yearMade = (node \ "yearMade").text.toInt
      val dateObtained = (node \ "dateObtained").text
      val bookPrice = (node \ "bookPrice").text.toInt
      val purchasePrice = (node \ "purchasePrice").text.toInt
      val condition = (node \ "condition").text.toInt
    }
  }

  def coke = new CCTherm {
    val description = "hot dog #5"
    val yearMade = 1952
    val dateObtained = "March 14, 2006"
    val bookPrice = 2199
    val purchasePrice = 500
    val condition = 9
  }

}