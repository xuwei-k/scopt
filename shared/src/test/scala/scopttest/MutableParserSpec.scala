package scopttest

import java.io.ByteArrayOutputStream

object MutableParserSpec extends verify.BasicTestSuite {

  test("unit parser should parse ()") {
    unitParser("--foo")
    unitParser("-f")
  }

  test("int parser should parse 1") {
    intParser("--foo", "1")
    intParser("--foo:1")
    intParser("--foo=1")
    intParser("-f", "1")
    intParser("-f:1")
    intParser("-f=1")
    intParser("--foo", "0x01")
    intParser("--foo:0x01")
    intParser("--foo=0x01")
    intParser("-f", "0x1")
    intParser("-f:0x1")
    intParserFail { "--foo" }
    intParserFail("--foo", "bar")
    intParserFail("--foo=bar")
  }

  test("long parser should parse 1") {
    longParser("--foo", "1")
    longParser("--foo:1")
    longParser("--foo=1")
    longParser("-f", "1")
    longParser("-f:1")
    longParser("-f=1")
    longParser("--foo", "0x01")
    longParser("--foo:0x01")
    longParser("--foo=0x01")
    longParser("-f", "0x1")
    longParser("-f:0x1")
  }

  test("string parser should parse bar") {
    stringParser("--foo", "bar")
    stringParser("--foo:bar")
    stringParser("--foo=bar")
  }

  test("double parser should parse 1.0") {
    doubleParser("--foo", "1.0")
    doubleParser("--foo:1.0")
    doubleParser("--foo=1.0")
    doubleParserFail("--foo", "bar")
    doubleParserFail("--foo=bar")
  }

  test("boolean parser should parse true") {
    trueParser("--foo", "true")
    trueParser("--foo:true")
    trueParser("--foo=true")
    trueParser("--foo", "1")
    trueParser("--foo:1")
    boolParserFail("--foo", "bar")
    boolParserFail("--foo=bar")
  }

  test("pair parse should parse (k, 1)") {
    pairParser("--foo", "k=1")
    pairParser("--foo:k=1")
    pairParser("--foo=k=1")
    pairParserFail("--foo")
    pairParserFail("--foo", "bar")
    pairParserFail("--foo", "k=bar")
    pairParserFail("--foo=k=bar")
  }

  test(".required() should fail when the option is missing") {
    requiredFail()
  }

  test(".required().withFallback() should parse the provided value") {
    requiredWithFallback(args = Seq("--stringValue", "provided"), expected = "provided")
  }

  test(".required().withFallback() should use the fallback value") {
    requiredWithFallback(args = Nil, expected = "someFallback")
  }

  test(".hidden() option should still parse ()") {
    unitParserHidden("--debug")
  }

  test("unknown options should fail to parse by default") {
    intParserFail("-z", "bar")
  }

  test("validate should fail to parse --foo 0") {
    validFail("--foo", "0")
  }

  test("int argument should parse 80") {
    intArg("80")
    intArgFail()
  }

  test("string argument should parse a string") {
    multipleArgs("a", "b")
  }

  test("unbounded() should parse strings") {
    unboundedArgs("a", "b")
    emptyArgs()
  }

  test("command") {
    cmdParser("update")
  }

  test("help with one column") {
    helpParserOneColumn()
  }

  test("help with two columns") {
    helpParserTwoColumns()
  }

  test("reportError should print error") {
    reportErrorParser("foo")
  }

  test("reportWarning should print warning") {
    reportWarningParser("foo")
  }

  test("showUsage") {
    showUsageParser()
  }

  test("withFallback doesn't use .toString") {
    val expectedConfig = NotToStringInverse("hi", 2)
    var config = NotToStringInverse.empty
    val parser = new scopt.OptionParser[Unit]("withFallback") {
      head("withFallback")
      opt[NotToStringInverse]('x', "x")
        .withFallback(() => expectedConfig)
        .foreach(value => config = value)
    }

    assert(parser.parse(Seq(), ()).isDefined)
    assert(config == expectedConfig)
  }

  import SpecUtil._

  def unitParser(args: String*): Unit = {
    var foo = false
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Unit]('f', "foo").foreach(_ => foo = true)
      help("help")
    }
    val result = parser.parse(args.toSeq, ()).isDefined
    assert(result && foo)
  }

  def unitParserHidden(args: String*): Unit = {
    var debug = false
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Unit]("debug").hidden().foreach(_ => debug = true)
      help("help")
    }
    val result = parser.parse(args.toSeq, ()).isDefined
    assert(debug)
  }

  def intParser(args: String*): Unit = {
    var foo = 0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Int]('f', "foo").foreach(x => foo = x)
      help("help")
    }
    parser.parse(args.toSeq, ()).isDefined
    assert(foo == 1)
  }

  def longParser(args: String*): Unit = {
    var foo = 0L
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Long]('f', "foo").foreach(x => foo = x)
      help("help")
    }
    parser.parse(args.toSeq, ()).isDefined
    assert(foo == 1L)
  }

  def intParserFail(args: String*): Unit = {
    var foo = 0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Int]('f', "foo").foreach(x => foo = x)
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined == false)
  }

  def stringParser(args: String*): Unit = {
    var foo = ""
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[String]("foo").foreach(x => foo = x)
      help("help")
    }
    parser.parse(args.toSeq, ())
    assert(foo == "bar")
  }

  def doubleParser(args: String*): Unit = {
    var foo = 0.0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Double]("foo").foreach(x => foo = x)
      help("help")
    }
    parser.parse(args.toSeq, ())
    assert(foo == 1.0)
  }

  def doubleParserFail(args: String*): Unit = {
    var foo = 0.0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Double]("foo").foreach(x => foo = x)
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined == false)
  }

  def trueParser(args: String*): Unit = {
    var foo = false
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Boolean]("foo").foreach(x => foo = x)
      help("help")
    }
    parser.parse(args.toSeq, ())
    assert(foo == true)
  }

  def boolParserFail(args: String*): Unit = {
    var foo = false
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Boolean]("foo").foreach(x => foo = x)
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined == false)
  }

  def pairParser(args: String*): Unit = {
    var foo = ""
    var value = 0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[(String, Int)]("foo").foreach({ case (k, v) =>
        foo = k
        value = v
      })
      help("help")
    }
    parser.parse(args.toSeq, ())
    assert((foo == "k") && (value == 1))
  }

  def pairParserFail(args: String*): Unit = {
    var foo = ""
    var value = 0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[(String, Int)]("foo").foreach({ case (k, v) =>
        foo = k
        value = v
      })
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined == false)
  }

  def requiredFail(args: String*): Unit = {
    var foo = ""
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[String]("foo").required().foreach(x => foo = x)
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined == false)
  }

  def requiredWithFallback(args: Seq[String], expected: String): Unit = {
    var stringValue = ""
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[String]("stringValue")
        .required()
        .withFallback(() => "someFallback")
        .foreach(x => stringValue = x)
      help("help")
    }
    assert {
      parser.parse(args, ()).isDefined && stringValue == expected
    }
  }

  def validFail(args: String*): Unit = {
    var foo = 0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      opt[Int]('f', "foo")
        .foreach(x => foo = x)
        .validate(x =>
          if (x > 0) success
          else failure("Option --foo must be >0")
        )
        .validate(x => failure("Just because"))
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined == false)
  }

  def intArg(args: String*): Unit = {
    var port = 0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[Int]("<port>").foreach(x => port = x)
      help("help")
    }
    parser.parse(args.toSeq, ())
    assert(port == 80)
  }

  def intArgFail(args: String*): Unit = {
    var port = 0
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[Int]("<port>").foreach(x => port = x)
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined == false)
  }

  def multipleArgs(args: String*): Unit = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[String]("<a>").foreach(x => a = x)
      arg[String]("<b>").foreach(x => b = x)
      help("help")
    }
    parser.parse(args.toSeq, ())
    assert((a == "a") && (b == "b"))
  }

  def unboundedArgs(args: String*): Unit = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[String]("<a>").foreach(x => a = x).unbounded()
      arg[String]("<b>").foreach(x => b = x)
      help("help")
    }
    parser.parse(args.toSeq, ())
    assert((a == "b") && (b == ""))
  }

  def emptyArgs(args: String*): Unit = {
    var a = ""
    var b = ""
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      arg[String]("<a>").foreach(x => a = x).unbounded().optional()
      arg[String]("<b>").foreach(x => b = x).optional()
      help("help")
    }
    assert(parser.parse(args.toSeq, ()).isDefined)
  }

  def cmdParser(args: String*): Unit = {
    var foo = false
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      cmd("update").foreach(_ => foo = true)
      help("help")
    }
    val result = parser.parse(args.toSeq, ()).isDefined
    assert(result && foo)
  }

  def helpParserOneColumn(args: String*): Unit = {
    case class Config(
        foo: Int = -1,
        xyz: Boolean = false,
        libName: String = "",
        maxCount: Int = -1,
        verbose: Boolean = false,
        debug: Boolean = false,
        mode: String = "",
        keepalive: Boolean = false
    )
    var c = Config()
    val parser = new scopt.OptionParser[Unit]("scopt") {
      override def renderingMode: scopt.RenderingMode = scopt.RenderingMode.OneColumn
      head("scopt", "3.x")

      opt[Int]('f', "foo").foreach(x => c = c.copy(foo = x)).text("foo is an integer property")

      opt[(String, Int)]("max")
        .foreach({ case (k, v) =>
          c = c.copy(libName = k, maxCount = v)
        })
        .validate(x =>
          if (x._2 > 0) success
          else failure("Value <max> must be >0")
        )
        .keyValueName("<libname>", "<max>")
        .text("maximum count for <libname>")

      opt[Unit]("verbose").foreach(_ => c = c.copy(verbose = true)).text("verbose is a flag")

      opt[Unit]("debug")
        .hidden()
        .foreach(_ => c = c.copy(debug = true))
        .text("this option is hidden in the usage text")

      help("help").text("prints this usage text")

      note("some notes.".newline)

      cmd("update")
        .foreach(_ => c.copy(mode = "update"))
        .text("update is a command.")
        .children(
          opt[Unit]("not-keepalive")
            .abbr("nk")
            .foreach(_ => c.copy(keepalive = false))
            .text("disable keepalive"),
          opt[Boolean]("xyz").foreach(x => c = c.copy(xyz = x)).text("xyz is a boolean property"),
          opt[Unit]("debug-update")
            .hidden()
            .foreach(_ => c = c.copy(debug = true))
            .text("this option is hidden in the usage text")
        )
    }
    parser.parse(args.toSeq, ())
    assert(parser.usage == """scopt 3.x
Usage: scopt [update] [options]

  -f <value> | --foo <value>
        foo is an integer property
  --max:<libname>=<max>
        maximum count for <libname>
  --verbose
        verbose is a flag
  --help
        prints this usage text
some notes.

Command: update [options]
update is a command.
  -nk | --not-keepalive
        disable keepalive
  --xyz <value>
        xyz is a boolean property""")
  }

  def helpParserTwoColumns(args: String*): Unit = {
    case class Config(
        foo: Int = -1,
        xyz: Boolean = false,
        libName: String = "",
        maxCount: Int = -1,
        verbose: Boolean = false,
        debug: Boolean = false,
        mode: String = "",
        keepalive: Boolean = false
    )
    var c = Config()
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")

      opt[Int]('f', "foo").foreach(x => c = c.copy(foo = x)).text("foo is an integer property")

      opt[(String, Int)]("max")
        .foreach({ case (k, v) =>
          c = c.copy(libName = k, maxCount = v)
        })
        .validate(x =>
          if (x._2 > 0) success
          else failure("Value <max> must be >0")
        )
        .keyValueName("<libname>", "<max>")
        .text("maximum count for <libname>")

      opt[Unit]("verbose").foreach(_ => c = c.copy(verbose = true)).text("verbose is a flag")

      opt[Unit]("debug")
        .hidden()
        .foreach(_ => c = c.copy(debug = true))
        .text("this option is hidden in the usage text")

      help("help").text("prints this usage text")

      note("some notes.".newline)

      cmd("update")
        .foreach(_ => c.copy(mode = "update"))
        .text("update is a command.")
        .children(
          opt[Unit]("not-keepalive")
            .abbr("nk")
            .foreach(_ => c.copy(keepalive = false))
            .text("disable keepalive"),
          opt[Boolean]("xyz").foreach(x => c = c.copy(xyz = x)).text("xyz is a boolean property"),
          opt[Unit]("debug-update")
            .hidden()
            .foreach(_ => c = c.copy(debug = true))
            .text("this option is hidden in the usage text")
        )
    }
    parser.parse(args.toSeq, ())
    assert(parser.usage == """scopt 3.x
Usage: scopt [update] [options]

  -f, --foo <value>        foo is an integer property
  --max:<libname>=<max>    maximum count for <libname>
  --verbose                verbose is a flag
  --help                   prints this usage text
some notes.

Command: update [options]
update is a command.
  -nk, --not-keepalive     disable keepalive
  --xyz <value>            xyz is a boolean property""")
  }

  def printParserError(body: scopt.OptionParser[Unit] => Unit): String = {
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      help("help") text ("prints this usage text")
      override def terminate(exitState: Either[String, Unit]): Unit = ()
    }
    val bos = new ByteArrayOutputStream()
    Console.withErr(bos) { body(parser) }
    bos.toString("UTF-8")
  }
  def printParserOut(body: scopt.OptionParser[Unit] => Unit): String = {
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "3.x")
      help("help") text ("prints this usage text")
      override def terminate(exitState: Either[String, Unit]): Unit = ()
    }
    val bos = new ByteArrayOutputStream()
    Console.withOut(bos) { body(parser) }
    bos.toString("UTF-8")
  }
  def reportErrorParser(msg: String): Unit = {
    assert(printParserError(_.reportError(msg)) == "Error: foo".newline)
  }
  def reportWarningParser(msg: String): Unit = {
    assert(printParserError(_.reportWarning(msg)) == "Warning: foo".newline)
  }
  def showUsageParser(): Unit = {
    assert(printParserOut(_.parse(List("--help"), ())) == """scopt 3.x
Usage: scopt [options]

  --help  prints this usage text
""")
  }
}
