import org.specs2._

class ImmutableParserSpec extends Specification { def is =      s2"""
  This is a specification to check the immutable parser
  
  opt[Unit]('f', "foo") action { x => x } should
    parse () out of --foo                                       ${unitParser("--foo")}
    parse () out of -f                                          ${unitParser("-f")} 

  help("help") should
    print usage text --help                                     ${helpParser("--help")}
                                                                """

  def unitParser(args: String*) = {
    val parser = new scopt.immutable.OptionParser[Config]("scopt", "3.x") { def options = Seq(
      opt[Unit]('f', "foo") action { (x, c) => c.copy(flag = true) }
    ) }
    val result = parser.parse(args.toSeq, Config())
    (result.get.flag === true)
  }

  def helpParser(args: String*) = {
    case class Config(foo: Int = -1, out: String = "", xyz: Boolean = false,
      libName: String = "", maxCount: Int = -1, verbose: Boolean = false,
      mode: String = "", files: Seq[String] = Seq())
    val parser = new scopt.immutable.OptionParser[Config]("scopt", "3.x") { def options = Seq(
      opt[Int]('f', "foo") action { (x, c) =>
        c.copy(foo = x) } text("foo is an integer property"),
      opt[String]('o', "out") required() valueName("<file>") action { (x, c) =>
        c.copy(out = x) } text("out is a required string property"),
      opt[Boolean]("xyz") action { (x, c) =>
        c.copy(xyz = x) } text("xyz is a boolean property"),
      opt[(String, Int)]("max") action { case ((k, v), c) =>
        c.copy(libName = k, maxCount = v) } validate { x =>
        if (x._2 > 0) success else failure("Value <max> must be >0") 
      } keyValueName("<libname>", "<max>") text("maximum count for <libname>"),
      opt[Unit]("verbose") action { (_, c) =>
        c.copy(verbose = true) } text("verbose is a flag"),
      note("some notes.\n"),
      help("help") text("prints this usage text"),
      arg[String]("<mode>") required() action { (x, c) =>
        c.copy(mode = x) } text("required argument"),
      arg[String]("<file>...") unbounded() action { (x, c) =>
        c.copy(files = c.files :+ x) } text("optional unbounded args")
    ) }
    parser.parse(args.toSeq, Config())
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

  case class Config(flag: Boolean = false, intValue: Int = 0)
}
