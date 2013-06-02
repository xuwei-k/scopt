import org.specs2._

class MutableParserSpec extends Specification { def is =      s2"""
  This is a specification to check the mutable parser

  opt[Int]("foo") action { x => x } should
    parse 1 out of --foo 1                                      $e1
  
  opt[String]("foo") action { x => x } should
    parse "bar" out of --foo bar                                $e2

  opt[Double]("foo") action { x => x } should
    parse 1.0 out of --foo 1.0                                  $e3

  opt[Boolean]("foo") action { x => x } should
    parse true out of --foo 1                                   $e4

  opt[(String, Int)]("foo") action { x => x } should
    parse ("k", 1) out of --foo k=1                             $e5    
                                                                """

  def e1 = {
    var foo = 0
    val parser1 = new scopt.OptionParser("scopt") {
      opt[Int]("foo") action { x => foo = x }
    }
    parser1.parse(Seq("--foo", "1"))
    foo === 1
  }

  def e2 = {
    var foo = ""
    val parser1 = new scopt.OptionParser("scopt") {
      opt[String]("foo") action { x => foo = x }
    }
    parser1.parse(Seq("--foo", "bar"))
    foo === "bar"
  }

  def e3 = {
    var foo = 0.0
    val parser1 = new scopt.OptionParser("scopt") {
      opt[Double]("foo") action { x => foo = x }
    }
    parser1.parse(Seq("--foo", "1.0"))
    foo === 1.0
  }

  def e4 = {
    var foo = 0.0
    val parser1 = new scopt.OptionParser("scopt") {
      opt[Double]("foo") action { x => foo = x }
    }
    parser1.parse(Seq("--foo", "1"))
    foo === 1.0
  }

  def e5 = {
    var foo = ""
    var value = 0
    val parser1 = new scopt.OptionParser("scopt") {
      opt[(String, Int)]("foo") action { case (k, v) =>
        foo = k
        value = v
      }
    }
    parser1.parse(Seq("--foo", "k=1"))
    (foo === "k") and (value === 1)
  }
}
