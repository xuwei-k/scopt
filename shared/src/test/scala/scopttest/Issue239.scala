package scopttest

object Issue239 extends verify.BasicTestSuite {
  test("double arg should accept negative numbers") {
    val set = List("-3.1415926" -> -3.1415926, "-.1" -> -.1)
    val p = new scopt.OptionParser[Double]("Test") {
      opt[String]('n', "name")
      arg[Double]("value").action((v, _) => v)
    }
    set.foreach { case (arg, expected) =>
      val res = p.parse(Array("--", arg), Double.NaN)
      assert(res == Some(expected))
    }
    ()
  }

  test("int arg should accept negative numbers") {
    val set = List("-3" -> -3, "-0" -> 0)
    val p = new scopt.OptionParser[Int]("Test") {
      opt[String]('n', "name")
      arg[Int]("value").action((v, _) => v)
    }
    set.foreach { case (arg, expected) =>
      val res = p.parse(Array("--", arg), Int.MaxValue)
      assert(res == Some(expected))
    }
    ()
  }

  test("double opt should accept negative numbers") {
    val set = List("-3.1415926" -> -3.1415926, "-.1" -> -.1)
    set.foreach { case (arg, expected) =>
      val p = new scopt.OptionParser[Double]("Test") {
        opt[Double]('v', "value").action((v, _) => v)
      }
      val res = p.parse(Array("-v", arg), Double.NaN)
      assert(res == Some(expected))
    }
    ()
  }

  test("int opt should accept negative numbers") {
    val set = List("-3" -> -3, "-0" -> 0)
    set.foreach { case (arg, expected) =>
      val p = new scopt.OptionParser[Int]("Test") {
        opt[Int]('v', "value").action((v, _) => v)
      }
      val res = p.parse(Array("--value", arg), Int.MaxValue)
      assert(res == Some(expected))
    }
    ()
  }

  test("support -1 as option arguments, short option, and argument (operand)") {
    var asShortOption = false
    var optionArgument = 0
    var argument = 0

    val args = "cmd --name -1 -1 -- -1".split(' ').drop(1)
    val parser = new scopt.OptionParser[Unit]("scopt") {
      head("scopt", "4.x")
      opt[Unit]('1', "one").foreach { _ =>
        asShortOption = true
      }
      opt[Int]("name").foreach(optionArgument = _)
      arg[Int]("<int>").foreach(argument = _)
    }

    parser.parse(args, ())

    assert(asShortOption && optionArgument == -1 && argument == -1)
  }

  test("group single digit options") {
    val parser = new scopt.OptionParser[String]("scopt") {
      head("scopt", "4.x")
      opt[Unit]('1', "one").action((_, s) => s + '1')
      opt[Unit]('2', "two").action((_, s) => s + '2')
      opt[Unit]('3', "three").action((_, s) => s + '3')
      opt[Int]('o', "other").action((_, s) => s + 'o')
    }

    // FIXME: should also accept one final option taking an argument when grouping
    // val result = parser.parse(Seq("-123o", "4"), "")
    val result = parser.parse(Seq("-123", "-o", "4"), "")

    assert(result.isDefined && result.get == "123o")
  }
}
