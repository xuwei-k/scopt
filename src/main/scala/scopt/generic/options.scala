package scopt.generic

import collection.mutable.{ListBuffer, ListMap}

trait Read[A] {
  def tokensToRead: Int 
  def arity: Int
  def reads: String => A
}
object Read {
  def reads[A](f: String => A): Read[A] = new Read[A] {
    val tokensToRead = 1
    val arity = 1
    val reads = f
  }
  implicit val intRead: Read[Int]             = reads { _.toInt }
  implicit val stringRead: Read[String]       = reads { identity }
  implicit val doubleRead: Read[Double]       = reads { _.toDouble }
  implicit val booleanRead: Read[Boolean]     =
    reads { _.toLowerCase match {
      case "true"  => true
      case "false" => false
      case "yes"   => true
      case "no"    => false
      case "1"     => true
      case "0"     => false
      case s       =>
        throw new IllegalArgumentException("'" + s + "' is not a boolean.")
    }}
  implicit def tupleRead[A1: Read, A2: Read]: Read[(A1, A2)] = new Read[(A1, A2)] {
    val tokensToRead = 1
    val arity = 2
    val reads = { (s: String) =>
      splitKeyValue(s) match {
        case (k, v) => implicitly[Read[A1]].reads(k) -> implicitly[Read[A2]].reads(v)
      }
    }
  } 
  private def splitKeyValue(s: String): (String, String) =
    s.indexOf('=') match {
      case -1     => throw new IllegalArgumentException("Expected a key=value pair")
      case n: Int => (s.slice(0, n), s.slice(n + 1, s.length))
    }
  implicit val unitRead: Read[Unit] = new Read[Unit] { 
    val tokensToRead = 0
    val arity = 0
    val reads = { (s: String) => () }
  }
}

trait OptionDefKind {}
case object Opt extends OptionDefKind
case object Arg extends OptionDefKind
case object Sep extends OptionDefKind

private[scopt] abstract class OptionDefinition[A: Read, C] {
  import OptionDefinition._

  def read: Read[A] = implicitly[Read[A]]
  def kind: OptionDefKind
  def name: String
  def _shortOpt: Option[Char]
  def _keyName: Option[String]
  def _valueName: Option[String]
  def _desc: String
  def shortOptOrBlank: String = _shortOpt map {_.toString} getOrElse("")
  def getMaxOccurs: Int
  def getMinOccurs: Int

  def callback: (A, C) => C
  def applyArgument(arg: String, config: C): Either[String, C] =
    try {
      Right(callback(read.reads(arg), config))
    } catch {
      case e: NumberFormatException => Left(shortDescription.capitalize + " expects a number but was given '" + arg + "'")
      case e: Throwable             => Left(shortDescription.capitalize + " failed when given '" + arg + "'. " + e.getMessage)
    }
  // number of tokens to read: 0 or no match, 2 for "--foo 1", 1 for "--foo:1"
  def shortOptTokens(arg: String): Int =
    _shortOpt match {
      case Some(c) if arg == "-" + shortOptOrBlank                 => 1 + read.tokensToRead
      case Some(c) if arg startsWith ("-" + shortOptOrBlank + ":") => 1
      case _ => 0
    }
  def longOptTokens(arg: String): Int =
    if (arg == fullName) 1 + read.tokensToRead
    else if (arg startsWith (fullName + ":")) 1
    else 0
  def tokensToRead(i: Int, args: Seq[String]): Int =
    if (i >= args.length || kind != Opt) 0
    else args(i) match {
      case arg if longOptTokens(arg) > 0  => longOptTokens(arg)
      case arg if shortOptTokens(arg) > 0 => shortOptTokens(arg) 
      case _ => 0
    }
  def apply(i: Int, args: Seq[String]): Either[String, String] =
    if (i >= args.length || kind != Opt) Left("Option does not match")
    else args(i) match {
      case arg if longOptTokens(arg) == 2 || shortOptTokens(arg) == 2 =>
        token(i + 1, args) map {Right(_)} getOrElse Left("Missing value after " + arg)
      case arg if longOptTokens(arg) == 1 && read.tokensToRead == 1 =>
        Right(arg drop (fullName + ":").length)
      case arg if shortOptTokens(arg) == 1 && read.tokensToRead == 1 =>
        Right(arg drop ("-" + shortOptOrBlank + ":").length)
      case _ => Right("")
    }
  def token(i: Int, args: Seq[String]): Option[String] =
    if (i >= args.length || kind != Opt) None
    else Some(args(i))
  def usage: String =
    kind match {
      case Sep => _desc
      case Arg => name + NLTB + _desc
      case Opt if read.arity == 2 =>
        (_shortOpt map { o => "-" + o + ":" + keyValueString + " | " } getOrElse { "" }) +
        fullName + ":" + keyValueString + NLTB + _desc
      case Opt if read.arity == 1 =>
        (_shortOpt map { o => "-" + o + " " + valueString + " | " } getOrElse { "" }) +
        fullName + " " + valueString + NLTB + _desc
      case Opt =>
        (_shortOpt map { o => "-" + o + " | " } getOrElse { "" }) + 
        fullName + NLTB + _desc    
    }    
  def keyValueString: String = (_keyName getOrElse defaultKeyName) + "=" + valueString 
  def valueString: String = (_valueName getOrElse defaultValueName)
  def shortDescription: String =
    kind match {
      case Opt => "option " + fullName
      case _   => "argument " + fullName
    }
  def fullName: String =
    kind match {
      case Opt => "--" + name
      case _   => name
    }
  def argName: String =
    kind match {
      case Arg if getMinOccurs == 0 => "[" + fullName + "]" 
      case _   => fullName
    }
}

private[scopt] object OptionDefinition {
  val UNBOUNDED = 1024
  val NL = System.getProperty("line.separator")
  val TB = "        "
  val NLTB = NL + TB
  val NLNL = NL + NL
  val defaultKeyName = "<key>"
  val defaultValueName = "<value>"
  val atomic = new java.util.concurrent.atomic.AtomicInteger
  def generateId: Int = atomic.incrementAndGet
}

private[scopt] trait GenericOptionParser[C] {
  def options: Seq[OptionDefinition[_, C]]
  def version: Option[String]
  def programName: Option[String]
  def errorOnUnknownArgument: Boolean

  protected def nonArgs: Seq[OptionDefinition[_, C]] = options filter {_.kind != Arg}
  protected def arguments: Seq[OptionDefinition[_, C]] = options filter {_.kind == Arg}

  def usage: String = {
    import OptionDefinition._
    val prorgamText = programName map { _ + " " } getOrElse { "" }
    val versionText = programName map { pg =>
      version map { NL + pg + " " + _ } getOrElse { "" }
    } getOrElse { "" }
    val optionText = if (nonArgs.isEmpty) {""} else {"[options] "}
    val argumentList = arguments map {_.argName} mkString(" ")
    val descriptions = (nonArgs map {_.usage}) ++ (arguments map {_.usage})

    versionText + NL + "Usage: " + prorgamText + optionText + argumentList + NLNL +
    "  " + descriptions.mkString(NL + "  ") + NL
  }

  def showUsage = Console.err.println(usage)
    
  /** parses the given `args`.
   */
  def parse(args: Seq[String], init: C): Option[C] = {
    var i = 0
    val pendingArgs = ListBuffer() ++ arguments
    val pendingOptions = ListBuffer() ++ nonArgs
    val occurrences = ListMap[OptionDefinition[_, C], Int]().withDefaultValue(0)
    var _config: C = init
    var _error = false

    def handleError(msg: String) {
      if (errorOnUnknownArgument) {
        System.err.println("Error: " + msg)
        _error = true
      }
      else System.err.println("Warning: " + msg)
    }
    def handleArgument(opt: OptionDefinition[_, C], arg: String) {
      opt.applyArgument(arg, _config) match {
        case Right(c)  => _config = c
        case Left(msg) =>
          _error = true
          System.err.println("Error: " + msg)
      }
    }
    while (i < args.length) {
      pendingOptions find {_.tokensToRead(i, args) > 0} match {
        case Some(option) =>
          occurrences(option) += 1
          if (occurrences(option) >= option.getMaxOccurs) {
            pendingOptions -= option
          }
          option(i, args) match {
            case Right(v) =>          handleArgument(option, v)
            case Left(outOfBounds) => handleError(outOfBounds)
          }
          // move index forward for gobbling
          if (option.tokensToRead(i, args) > 1) {
            i += option.tokensToRead(i, args) - 1
          } // if
        case None =>
          args(i) match {
            case arg if arg startsWith "-"  => handleError("Unknown option " + arg)
            case arg if pendingArgs.isEmpty => handleError("Unknown argument '" + arg + "'")
            case arg =>
              val first = pendingArgs.head
              occurrences(first) += 1
              if (occurrences(first) >= first.getMaxOccurs) {
                pendingArgs.remove(0)
              }
              handleArgument(first, arg)
          }
      }
      i += 1
    }
    (pendingOptions filter { opt => opt.getMinOccurs > occurrences(opt) }) foreach { opt =>
      if (opt.getMinOccurs == 1) System.err.println("Error: Missing " + opt.shortDescription)
      else System.err.println("Error: " + opt.shortDescription.capitalize + " must be given " + opt.getMinOccurs + " times")
      _error = true
    }
    (pendingArgs filter { arg => arg.getMinOccurs > occurrences(arg) }) foreach { arg =>
      if (arg.getMinOccurs == 1) System.err.println("Error: Missing " + arg.shortDescription)
      else System.err.println("Error: '" + arg.shortDescription.capitalize + "' must be given " + arg.getMinOccurs + " times")
      _error = true
    }
    if (_error) {
      showUsage
      None
    }
    else Some(_config)
  }
}
