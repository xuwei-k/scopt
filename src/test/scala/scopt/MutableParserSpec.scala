import org.specs2._

class MutableParserSpec extends Specification { def is =      s2"""
  This is a specification to check the mutable parser

  opt[Unit]('f', "foo") action { x => x } should
    parse () out of --foo                                       ${unitParser("--foo")}
    parse () out of -f                                          ${unitParser("-f")}

  opt[Int]('f', "foo") action { x => x } should
    parse 1 out of --foo 1                                      ${intParser("--foo", "1")}
    parse 1 out of --foo:1                                      ${intParser("--foo:1")}
    parse 1 out of -f 1                                         ${intParser("-f", "1")}
    parse 1 out of -f:1                                         ${intParser("-f:1")}
    fail to parse --foo                                         ${intParserFail{"--foo"}}
    fail to parse --foo bar                                     ${intParserFail("--foo", "bar")}

  opt[String]("foo") action { x => x } should
    parse "bar" out of --foo bar                                ${stringParser("--foo", "bar")}
    parse "bar" out of --foo:bar                                ${stringParser("--foo:bar")}

  opt[Double]("foo") action { x => x } should
    parse 1.0 out of --foo 1.0                                  ${doubleParser("--foo", "1.0")}
    parse 1.0 out of --foo:1.0                                  ${doubleParser("--foo:1.0")}
    fail to parse --foo bar                                     ${doubleParserFail("--foo", "bar")}

  opt[Boolean]("foo") action { x => x } should
    parse true out of --foo true                                ${trueParser("--foo", "true")}
    parse true out of --foo:true                                ${trueParser("--foo:true")}
    parse true out of --foo 1                                   ${trueParser("--foo", "1")}
    parse true out of --foo:1                                   ${trueParser("--foo:1")}
    fail to parse --foo bar                                     ${boolParserFail("--foo", "bar")}

  opt[(String, Int)]("foo") action { x => x } should
    parse ("k", 1) out of --foo k=1                             ${pairParser("--foo", "k=1")}
    parse ("k", 1) out of --foo:k=1                             ${pairParser("--foo:k=1")}
    fail to parse --foo                                         ${pairParserFail("--foo")}
    fail to parse --foo bar                                     ${pairParserFail("--foo", "bar")}
    fail to parse --foo k=bar                                   ${pairParserFail("--foo", "k=bar")}

  opt[String]("foo") required() action { x => x } should
    fail to parse Nil                                           ${requiredFail()}

  unknown options should
    fail to parse by default                                    ${intParserFail("-z", "bar")}

  opt[(String, Int)]("foo") action { x => x } validate { x =>
    if (x > 0) success else failure("Option --foo must be >0") } should
    fail to parse --foo 0                                       ${validFail("--foo", "0")}

  arg[Int]("<port>") required() action { x => x } should
    parse 80 out of 80                                          ${intArg("80")}
    be required and should fail to parse Nil                    ${intArgFail()}

  arg[String]("<a>"); arg[String]("<b>") action { x => x } should
    parse "b" out of a b                                        ${multipleArgs("a", "b")}

  arg[String]("<a>") action { x => x} unbounded(); arg[String]("<b>") should
    parse "b" out of a b                                        ${unboundedArgs("a", "b")}
    parse nothing out of Nil                                    ${emptyArgs()}

  help("help") should
    print usage text --help                                     ${helpParser("--help")}       
                                                                """

  def unitParser(args: String*) = {
    var foo = false
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Unit]('f', "foo") action { _ => foo = true }
    }
    val result = parser.parse(args.toSeq)
    (result === true) and (foo === true)
  }

  def intParser(args: String*) = {
    var foo = 0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Int]('f', "foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === 1
  }

  def intParserFail(args: String*) = {
    var foo = 0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Int]('f', "foo") action { x => foo = x }
    }
    parser.parse(args.toSeq) === false
  }

  def stringParser(args: String*) = {
    var foo = ""
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[String]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === "bar"
  }

  def doubleParser(args: String*) = {
    var foo = 0.0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Double]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === 1.0
  }

  def doubleParserFail(args: String*) = {
    var foo = 0.0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Double]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq) === false
  }

  def trueParser(args: String*) = {
    var foo = false
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Boolean]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq)
    foo === true
  }

  def boolParserFail(args: String*) = {
    var foo = false
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Boolean]("foo") action { x => foo = x }
    }
    parser.parse(args.toSeq) === false
  }

  def pairParser(args: String*) = {
    var foo = ""
    var value = 0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[(String, Int)]("foo") action { case (k, v) =>
        foo = k
        value = v
      }
    }
    parser.parse(args.toSeq)
    (foo === "k") and (value === 1)
  }

  def pairParserFail(args: String*) = {
    var foo = ""
    var value = 0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[(String, Int)]("foo") action { case (k, v) =>
        foo = k
        value = v
      }
    }
    parser.parse(args.toSeq) === false
  }

  def requiredFail(args: String*) = {
    var foo = ""
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[String]("foo") required() action { x => foo = x }
    }
    parser.parse(args.toSeq) === false
  }

  def validFail(args: String*) = {
    var foo = 0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      opt[Int]('f', "foo") action { x => foo = x } validate { x =>
        if (x > 0) success else failure("Option --foo must be >0") } validate { x =>
        failure("Just because") }
    }
    parser.parse(args.toSeq) === false
  }

  def intArg(args: String*) = {
    var port = 0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      arg[Int]("<port>") required() action { x => port = x }
    }
    parser.parse(args.toSeq)
    port === 80
  }

  def intArgFail(args: String*) = {
    var port = 0
    val parser = new scopt.OptionParser("scopt", "3.x") {
      arg[Int]("<port>") required() action { x => port = x }
    }
    parser.parse(args.toSeq) === false
  }

  def multipleArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser("scopt", "3.x") {
      arg[String]("<a>") action { x => a = x }
      arg[String]("<b>") action { x => b = x }
    }
    parser.parse(args.toSeq)
    (a === "a") and (b === "b")
  }

  def unboundedArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser("scopt", "3.x") {
      arg[String]("<a>") action { x => a = x } unbounded()
      arg[String]("<b>") action { x => b = x }
    }
    parser.parse(args.toSeq)
    (a === "b") and (b === "")
  }

  def emptyArgs(args: String*) = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser("scopt", "3.x") {
      arg[String]("<a>") action { x => a = x } unbounded()
      arg[String]("<b>") action { x => b = x }
    }
    parser.parse(args.toSeq) === true
  }

  def helpParser(args: String*) = {
    case class Config(foo: Int = -1, out: String = "", xyz: Boolean = false,
      libName: String = "", maxCount: Int = -1, verbose: Boolean = false,
      mode: String = "", files: Seq[String] = Seq())
    var c = Config()    
    val parser = new scopt.mutable.OptionParser("scopt", "3.x") {
      opt[Int]('f', "foo") action { x =>
        c = c.copy(foo = x) } text("foo is an integer property")
      opt[String]('o', "out") required() valueName("<file>") action { x =>
        c = c.copy(out = x) } text("out is a required string property")
      opt[Boolean]("xyz") action { x =>
        c = c.copy(xyz = x) } text("xyz is a boolean property")
      opt[(String, Int)]("max") action { case (k, v) =>
        c = c.copy(libName = k, maxCount = v) } validate { x =>
        if (x._2 > 0) success else failure("Value <max> must be >0") 
      } keyValueName("<libname>", "<max>") text("maximum count for <libname>")
      opt[Unit]("verbose") action { _ =>
        c = c.copy(verbose = true) } text("verbose is a flag")
      note("some notes.\n")
      help("help") text("prints this usage text")
      arg[String]("<mode>") required() action { x =>
        c = c.copy(mode = x) } text("required argument")
      arg[String]("<file>...") unbounded() action { x =>
        c = c.copy(files = c.files :+ x) } text("optional unbounded args")
    }
    parser.parse(args.toSeq)
    parser.usage === """
scopt 3.x
Usage: scopt [options] <mode> [<file>...]

  -f <value> | --foo <value>
        foo is an integer property
  -o <file> | --out <file>
        out is a required string property
  --xyz <value>
        xyz is a boolean property
  --max:<libname>=<max>
        maximum count for <libname>
  --verbose
        verbose is a flag
  some notes.

  --help
        prints this usage text
  <mode>
        required argument
  <file>...
        optional unbounded args
"""
  }
}
