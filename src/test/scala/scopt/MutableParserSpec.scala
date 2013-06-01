import org.specs2._

class MutableParserSpec extends Specification { def is =      s2"""
  This is a specification to check the mutable parser

  opt[Int]("foo") action { x => x } should
    parse --foo if provided                                     $e1

                                                                """

  def e1 = {
    var foo = 0
    val parser1 = new scopt.OptionParser("scopt") {
      opt[Int]("foo") action { x => foo = x }
    }
    parser1.parse(Seq("--foo", "1"))
    foo === 1
  }
}
