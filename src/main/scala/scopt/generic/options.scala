package scopt.generic

import GenericOptionParser.UNBOUNDED

trait OptionDefKind {}
case object Opt extends OptionDefKind
case object Arg extends OptionDefKind
case object Sep extends OptionDefKind

trait Read[A] {
  def tokensToRead: Int 
  def reads: String => A
}
object Read {
  def reads[A](f: String => A): Read[A] = new Read[A] {
    val tokensToRead = 1
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
        throw new IllegalArgumentException(s + "is not a boolean.")
    }}
  implicit def tupleRead[A1: Read, A2: Read]: Read[(A1, A2)] =
    reads {
      splitKeyValue(_) match {
        case (k, v) => implicitly[Read[A1]].reads(k) -> implicitly[Read[A2]].reads(v)
      }
    }
  private def splitKeyValue(s: String): (String, String) =
    s.indexOf('=') match {
      case -1     => throw new IllegalArgumentException("Expected a key=value pair")
      case n: Int => (s.slice(0, n), s.slice(n + 1, s.length))
    }
  implicit val unitRead: Read[Unit] = new Read[Unit] { 
    val tokensToRead = 0
    val reads = { (s: String) => () }
  }
}

private[scopt] abstract class OptionDefinition[A: Read, C] {
  def read: Read[A] = implicitly[Read[A]]
  def kind: OptionDefKind
  def name: Option[String]
  def nameOrBlank: String = name.getOrElse("")
  def _shortOpt: Option[Char]
  def shortOptOrBlank: String = _shortOpt map {_.toString} getOrElse("")
  def maxOccurs: Int
  def minOccurs: Int
  def shortDescription: String =
    kind match {
      case Opt => "option " + nameOrBlank
      case _   => "argument " + nameOrBlank
    }
  def callback: (A, C) => C
  def applyArgument(arg: String, config: C): Option[C]  =
    try {
      Some(callback(read.reads(arg), config))
    } catch {
      case e: NumberFormatException =>
        System.err.println("Error: " + shortDescription + " expects a number but was given '" + arg + "'")
        None
      case e: Throwable =>
        System.err.println("Error: " + shortDescription + " failed when given '" + arg + "'. " + e.getMessage)
        None
    }
  // number of tokens to read: 0 or no match, 2 for "--foo 1", 1 for "--foo:1"
  def shortOptTokens(arg: String): Int =
    _shortOpt match {
      case Some(c) if arg == "-" + shortOptOrBlank                 => 1 + read.tokensToRead
      case Some(c) if arg startsWith ("-" + shortOptOrBlank + ":") => 1
      case _ => 0
    }
  def longOptTokens(arg: String): Int =
    if (arg == "--" + nameOrBlank) 1 + read.tokensToRead
    else if (arg startsWith ("--" + nameOrBlank + ":")) 1
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
        token(i + 1, args) map {Right(_)} getOrElse Left("Missing value after '" + arg + "'")
      case arg if longOptTokens(arg) == 1 && read.tokensToRead == 1 =>
        Right(arg drop ("--" + nameOrBlank + ":").length)
      case arg if shortOptTokens(arg) == 1 && read.tokensToRead == 1 =>
        Right(arg drop ("-" + shortOptOrBlank + ":").length)
      case _ => Right("")
    }
  def token(i: Int, args: Seq[String]): Option[String] =
    if (i >= args.length || kind != Opt) None
    else Some(args(i))
}

// // ----- Some standard option types ---------
// private[scopt] class SeparatorDefinition[C](
//         description: String
//         ) extends OptionDefinition[C](false, null, null, null, null,
//           description, { (a: String, c: C) => c }, false, false, 1, 1)

// private[scopt] class Argument[C](
//         name: String,
//         description: String,
//         minOccurs: Int,
//         maxOccurs: Int,
//         action: (String, C) => C
//         ) extends OptionDefinition[C](false, null, name, null, name, 
//           description, action, false, false, minOccurs, maxOccurs) {

//   override def shortDescription = "argument " + name
// }

private[scopt] trait GenericOptionParser[C] {
  def options: Seq[OptionDefinition[_, C]]
  def version: Option[String]
  def programName: Option[String]
  def errorOnUnknownArgument: Boolean

  protected def opts: Seq[OptionDefinition[_, C]] = options filter {_.kind != Arg}
  protected def arguments: Seq[OptionDefinition[_, C]] = options filter { opt => (opt.kind == Arg) && opt.maxOccurs <= 1 }
  protected def argList: Option[OptionDefinition[_, C]] = options filter { opt => (opt.kind == Arg) && opt.maxOccurs > 1 } headOption

//   import GenericOptionParser._

//   // -------- Getting usage information ---------------
//   private def descriptions: Seq[String] = opts.map(opt => opt match {
//     //case x: Argument => x.longopt + " :" + NLTB + opt.description
//     case x if !x.canBeInvoked => x.description
//     case x if x.keyValueArgument =>
//       (x.shortopt map { o => "-" + o + ":" + x.keyName + "=" + x.valueName + " | " } getOrElse { "" }) +
//       "--" + x.longopt + ":" + x.keyName + "=" + x.valueName + NLTB + x.description    
//     case x if x.gobbleNextArgument =>
//       (x.shortopt map { o => "-" + o + " " + x.valueName + " | " } getOrElse { "" }) +
//       "--" + x.longopt + " " + x.valueName + NLTB + x.description
//     case _ =>
//       (opt.shortopt map { o => "-" + o + " | " } getOrElse { "" }) + 
//       "--" + opt.longopt + NLTB + opt.description
//   }) ++ (argList match {
//     case Some(x: Argument[C]) => List(x.valueName + NLTB + x.description)
//     case None                 => arguments.map(a => a.valueName + NLTB + a.description)
//   })
  
//   def usage: String = {
//     val prorgamText = programName map { _ + " " } getOrElse { "" }
//     val versionText = programName map { pg =>
//       version map { NL + pg + " " + _ } getOrElse { "" }
//     } getOrElse { "" }
//     val optionText = if (opts.isEmpty) {""} else {"[options] "}
//     val argumentList = argumentNames.mkString(" ")

//     versionText + NL + "Usage: " + prorgamText + optionText + argumentList + NLNL +
//     "  " + descriptions.mkString(NL + "  ") + NL
//   }

//   def showUsage = Console.err.println(usage)

//   private def argumentNames: Seq[String] = argList match {
//     case Some(x: Argument[C]) => List(x.valueName)
//     case None                 => arguments.map(_.valueName)
//   }
  
  /** parses the given `args`.
   */
  def parse(args: Seq[String], init: C): Option[C] = {
    import collection.mutable.ListBuffer

    var i = 0
    val unseenArgs = ListBuffer() ++ arguments
    var argListCount = 0
    var _config: C = init
    var _error = false

    while (i < args.length) {
      val arg = args(i)

      opts.find(_.tokensToRead(i, args) > 0) match {
        case None =>
      //     if (arg.startsWith("-")) {
      //       if (errorOnUnknownArgument) {
      //         System.err.println("Error: Unknown argument '" + arg + "'")
      //         _error = true              
      //       } else
      //         System.err.println("Warning: Unknown argument '" + arg + "'")
      //     } else if (argList.isDefined) {
      //       argListCount += 1
      //       applyArgument(argList.get, arg, _config) match {
      //         case Some(c) => _config = c
      //         case None    => _error = true
      //       }
      //     } else if (unseenArgs.isEmpty) {
      //       if (errorOnUnknownArgument) {
      //         System.err.println("Error: Unknown argument '" + arg + "'")
      //         _error = true             
      //       } else
      //         System.err.println("Warning: Unknown argument '" + arg + "'")
      //     } else {
      //       val first = unseenArgs.remove(0)
      //       applyArgument(first, arg, _config) match {
      //         case Some(c) => _config = c
      //         case None    => _error = true
      //       }
      //     }
          
        case Some(option) =>
          option(i, args) match {
            case Right(value) =>
              option.applyArgument(value, _config) match {
                case Some(c) => _config = c
                case None    => _error = true
              }
            case Left(outOfBounds) =>
              if (errorOnUnknownArgument) {
                System.err.println("Error: " + outOfBounds)
                _error = true
              }
              else System.err.println("Warning: " + outOfBounds)
          }
          // move index forward for gobbling
          if (option.tokensToRead(i, args) > 1) {
            i += option.tokensToRead(i, args) - 1
          } // if
      }
      i += 1
    }
    
    // if ((unseenArgs.toList exists { _.minOccurs > 0 }) ||
    //     (argListCount == 0 && (argList match {
    //       case Some(a: Argument[Unit]) => a.minOccurs > 0
    //       case _ => false
    //     }))) {
    //   System.err.println("Error: missing arguments: " + argumentNames.mkString(", "))
    //   _error = true
    // }
    // if (_error) {
    //   showUsage
    //   None
    // }
    // else Some(_config)

    None
  }
}

private[scopt] object GenericOptionParser {
  val UNBOUNDED = 1024
  val NL = System.getProperty("line.separator")
  val TB = "        "
  val NLTB = NL + TB
  val NLNL = NL + NL
  val defaultKeyName = "<key>"
  val defaultValueName = "<value>"
  var currentId = 0
  def generateId: Int = {
    val retval = currentId
    currentId = currentId + 1
    retval
  }
}
