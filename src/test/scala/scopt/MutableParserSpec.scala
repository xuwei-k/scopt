import org.specs2._

class MutableParserSpec extends Specification { def is =      s2"""
  This is a specification to check the mutable parser

  opt[Unit]("foo") action { x => x } should
    parse () out of --foo                                       ${unitParser("--foo")}

  opt[Int]("foo") action { x => x } should
    parse 1 out of --foo 1                                      ${intParser("--foo", "1")}
    parse 1 out of --foo:1                                      ${intParser("--foo:1")}
    
  opt[String]("foo") action { x => x } should
    parse "bar" out of --foo bar                                ${stringParser("--foo", "bar")}
    parse "bar" out of --foo:bar                                ${stringParser("--foo:bar")}

  opt[Double]("foo") action { x => x } should
    parse 1.0 out of --foo 1.0                                  ${doubleParser("--foo", "1.0")}
    parse 1.0 out of --foo:1.0                                  ${doubleParser("--foo:1.0")}

  opt[Boolean]("foo") action { x => x } should
    parse true out of --foo true                                ${trueParser("--foo", "true")}
    parse true out of --foo:true                                ${trueParser("--foo:true")}
    parse true out of --foo 1                                   ${trueParser("--foo", "1")}
    parse true out of --foo:1                                   ${trueParser("--foo:1")}

  opt[(String, Int)]("foo") action { x => x } should
    parse ("k", 1) out of --foo k=1                             ${pairParser("--foo", "k=1")}
    parse ("k", 1) out of --foo:k=1                             ${pairParser("--foo:k=1")}

  arg[Int]("<port>") action { x => x } should
    parse 80 out of 80                                          ${intArg("80")}

  arg[String]("<a>"); arg[String]("<b>") action { x => x } should
    parse "b" out of a b                                        ${multipleArgs("a", "b")}

  arg[String]("<a>") action { x => x} unbounded(); arg[String]("<b>") should
    parse "b" out of a b                                        ${unboundedArgs("a", "b")}

  help("help") should
    parse () out of --help                                      ${helpParser("--help")}       
                                                                """

  def unitParser(args: String*) = {
    var foo = false
    val parser = new scopt.OptionParser("scopt") {
      opt[Unit]("foo") action { _ => foo = true }
    }
    parser.parse(args.toSeq)
    foo === true
  }

  def intParser(args: String*) = {
    var foo = 0
    val parser = new scopt.OptionParser("scopt") {
      opt[Int]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === 1
  }

  def stringParser(args: String*) = {
    var foo = ""
    val parser = new scopt.OptionParser("scopt") {
      opt[String]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === "bar"
  }

  def doubleParser(args: String*) = {
    var foo = 0.0
    val parser = new scopt.OptionParser("scopt") {
      opt[Double]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === 1.0
  }

  def trueParser(args: String*) = {
    var foo = false
    val parser = new scopt.OptionParser("scopt") {
      opt[Boolean]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === true
  }

  def pairParser(args: String*) = {
    var foo = ""
    var value = 0
    val parser = new scopt.OptionParser("scopt") {
      opt[(String, Int)]("foo") action { case (k, v) =>
        foo = k
        value = v
      }
    }
    parser.parse(args.toSeq)
    (foo === "k") and (value === 1)
  }

  def intArg(args: String*) = {
    var port = 0
    val parser = new scopt.OptionParser("scopt") {
      arg[Int]("<port>") action { x => port = x }
    }
    parser.parse(args.toSeq)
    port === 80
  }

  def multipleArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser("scopt") {
      arg[String]("<a>") action { x => a = x }
      arg[String]("<b>") action { x => b = x }
    }
    parser.parse(args.toSeq)
    (a === "a") and (b === "b")
  }

  def unboundedArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser("scopt") {
      arg[String]("<a>") action { x => a = x } unbounded()
      arg[String]("<b>") action { x => b = x }
    }
    parser.parse(args.toSeq)
    (a === "b") and (b === "")
  }

  def helpParser(args: String*) = {
    val parser = new scopt.OptionParser("scopt") {
      help("help")
    }
    parser.parse(args.toSeq)
    success
  }
}
