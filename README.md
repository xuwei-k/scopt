scopt
=====

scopt is a little command line options parsing library.

Sonatype
--------

```scala
libraryDependencies += "com.github.scopt" %% "scopt" % "3.0.0-SNAPSHOT"

resolvers += "sonatype-public" at "https://oss.sonatype.org/content/groups/public"
```

Usage
-----

scopt provides two flavors of parsers: immutable and mutable.
Either case, first you need a case class that represents the configuration:

```scala
case class Config(foo: Int = -1, out: String = "", xyz: Boolean = false,
  libName: String = "", maxCount: Int = -1, verbose: Boolean = false,
  mode: String = "", files: Seq[String] = Seq())
```

An immutable parser lets you pass around a config object as an argument into callback closures.
On the other hand, the mutable parsers are expected to modify a config object in place.

### Immutable Parser

Here's how you create a `scopt.immutable.OptionParser`. See [Scaladoc API](http://scopt.github.com/scopt/latest/api/) for the details on various builder methods.

```scala
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
// parser.parse returns Option[C]
parser.parse(args, Config()) map { config =>
  // do stuff
} getOrElse {
  // arguments are bad, usage message will have been displayed
}
```

The above generates the following usage text:

```
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
```

### Mutable Parser

Create a `scopt.mutable.OptionParser` and customise it with the options you need, passing in functions to process each option or argument.

```scala
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
if (parser.parse(args)) {
  // do stuff
}
else {
  // arguments are bad, usage message will have been displayed
}
```

Building
--------

sbt to build scopt.

License
-------

MIT License.

Credits
-------

scopt 1.x was created by James Strachan based on the code from Tim Perrett,
which is based on Aaron Harnly's code mentioned [in this thread](http://old.nabble.com/-scala--CLI-library--ts19391923.html#a19391923) and [this thread](http://old.nabble.com/Parsing-command-lines-argument-in-a-%22scalaesque%22-way-tp26592006p26595257.html), which is available [as a gist](http://gist.github.com/246481) or [here](http://harnly.net/tmp/OptionsParser.scala).
Eugene Yokota joined the project, and forked to scopt/scopt for 2.x.

2.x added immutable parser, and 3.x was rewritten from scratch to support better polymophism and validation.

Changes
-------

See [notes](https://github.com/scopt/scopt/tree/master/notes).
