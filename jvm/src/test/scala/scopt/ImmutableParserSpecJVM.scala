package scopttest

import java.util.{ Calendar, GregorianCalendar }
import java.io.File
import java.net.{ InetAddress, URI, URL }
import scala.concurrent.duration.Duration
import scala.io.Source

object ImmutableParserSpecJVM extends verify.BasicTestSuite {

  private val url = new URI("https://example.com").toURL
  private val uri = new URI("https://example.com/robots.txt")

  test("calendar parser should parse 2000-01-01") {
    calendarParser("--foo", "2000-01-01")
    calendarParser("--foo=2000-01-01")
    calendarParserFail("--foo", "bar")
    calendarParserFail("--foo=bar")
  }

  test("file parser should parse test.txt") {
    fileParser("--foo", "test.txt")
    fileParser("--foo=test.txt")
  }

  test("source parser should parse test.txt") {
    sourceParser("--foo", "test.txt")
    sourceParser("--foo=test.txt")
  }

  test("InetAddress parser should parse 8.8.8.8") {
    inetAddressParser("--foo", "8.8.8.8")
    inetAddressParser("--foo=8.8.8.8")
  }

  test(s"UrlParser parser should parse $url") {
    urlParser("--foo", url.toString)
    urlParser(s"--foo=${url.toString}")
  }

  test(s"UriParser parser should parse $uri") {
    uriParser("--foo", uri.toString)
    uriParser(s"--foo=${uri.toString}")
  }

  import SpecUtil._

  val calendarParser1 = new scopt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[Calendar]("foo").action((x, c) => c.copy(calendarValue = x))
    help("help")
  }
  def calendarParser(args: String*): Unit = {
    val result = calendarParser1.parse(args.toSeq, Config())
    assert(
      result.get.calendarValue.getTime == new GregorianCalendar(2000, Calendar.JANUARY, 1).getTime
    )
  }
  def calendarParserFail(args: String*): Unit = {
    val result = calendarParser1.parse(args.toSeq, Config())
    assert(result == None)
  }

  val fileParser1 = new scopt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[File]("foo").action((x, c) => c.copy(fileValue = x))
    help("help")
  }
  def fileParser(args: String*): Unit = {
    val result = fileParser1.parse(args.toSeq, Config())
    assert(result.get.fileValue == new File("test.txt"))
  }

  val sourceParser1 = new scopt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[Source]("foo").action((x, c) => c.copy(sourceValue = x))
    help("help")
  }
  def sourceParser(args: String*): Unit = {
    val result = sourceParser1.parse(args.toSeq, Config())
    val resultSourceValue = result.get.sourceValue
      .getLines()
      .toSeq

    val expectedSourceValue =
      Source.fromFile("test.txt").getLines().toSeq

    assert(resultSourceValue == expectedSourceValue)
  }

  val inetAddressParser1 = new scopt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[InetAddress]("foo").action((x, c) => c.copy(inetAddressValue = x))
    help("help")
  }
  def inetAddressParser(args: String*): Unit = {
    val result = inetAddressParser1.parse(args.toSeq, Config())
    assert(result.get.inetAddressValue == InetAddress.getByName("8.8.8.8"))
  }

  val urlParser1 = new scopt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[URL]("foo").action((x, c) => c.copy(url = x))
    help("help")
  }
  def urlParser(args: String*): Unit = {
    val result = urlParser1.parse(args.toSeq, Config())
    assert(result.get.url == url)
  }

  val uriParser1 = new scopt.OptionParser[Config]("scopt") {
    head("scopt", "3.x")
    opt[URI]("foo").action((x, c) => c.copy(uri = x))
    help("help")
  }
  def uriParser(args: String*): Unit = {
    val result = uriParser1.parse(args.toSeq, Config())
    assert(result.get.uri == uri)
  }

  case class Config(
      flag: Boolean = false,
      intValue: Int = 0,
      longValue: Long = 0L,
      stringValue: String = "",
      doubleValue: Double = 0.0,
      boolValue: Boolean = false,
      debug: Boolean = false,
      bigDecimalValue: BigDecimal = BigDecimal("0.0"),
      calendarValue: Calendar = new GregorianCalendar(1900, Calendar.JANUARY, 1),
      fileValue: File = new File("."),
      sourceValue: Source = Source.fromChar(0),
      uriValue: URI = new URI("http://localhost"),
      inetAddressValue: InetAddress = InetAddress.getByName("0.0.0.0"),
      durationValue: Duration = Duration("0s"),
      key: String = "",
      a: String = "",
      b: String = "",
      seqInts: Seq[Int] = Seq(),
      mapStringToBool: Map[String, Boolean] = Map(),
      seqTupleStringString: Seq[(String, String)] = Nil,
      charValue: Char = 0,
      url: URL = url,
      uri: URI = uri
  )
}
