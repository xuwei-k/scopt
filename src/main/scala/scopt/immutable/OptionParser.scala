package scopt.immutable

import scopt.generic._

/** <code>scopt.immutable.OptionParser</code> is instantiated within your object,
 * set up by implementing 
 * <a href="options:Seq[OptionDefinition[C]]"><code>options</code></a> method that returns a sequence of invocations of 
 * the various builder methods such as
 * <a href="#opt(String,String,String)((String, C) ⇒ C):ArgOptionDefinition[C]"><code>opt</code></a> method or
 * <a href="#arg(String,String)((String, C) ⇒ C):Argument[C]"><code>arg</code></a> method.
 * {{{
 * val parser = new scopt.immutable.OptionParser[Config]("scopt", "3.x") { def options = Seq(
 *   opt[Int]('f', "foo") action { (x, c) =>
 *     c.copy(foo = x) } text("foo is an integer property"),
 *   opt[String]('o', "out") required() valueName("<file>") action { (x, c) =>
 *     c.copy(out = x) } text("out is a required string property"),
 *   opt[Boolean]("xyz") action { (x, c) =>
 *     c.copy(xyz = x) } text("xyz is a boolean property"),
 *   opt[(String, Int)]("max") action { case ((k, v), c) =>
 *     c.copy(libName = k, maxCount = v) } validate { x =>
 *     if (x._2 > 0) success else failure("Value <max> must be >0") 
 *   } keyValueName("<libname>", "<max>") text("maximum count for <libname>"),
 *   opt[Unit]("verbose") action { (_, c) =>
 *     c.copy(verbose = true) } text("verbose is a flag"),
 *   note("some notes.\n"),
 *   help("help") text("prints this usage text"),
 *   arg[String]("<mode>") required() action { (x, c) =>
 *     c.copy(mode = x) } text("required argument"),
 *   arg[String]("<file>...") unbounded() action { (x, c) =>
 *     c.copy(files = c.files :+ x) } text("optional unbounded args")
 * ) }
 * // parser.parse returns Option[C]
 * parser.parse(args, Config()) map { config =>
 *   // do stuff
 * } getOrElse {
 *   // arguments are bad, usage message will have been displayed
 * }
 * }}}
 */
abstract case class OptionParser[C](
    programName: Option[String] = None,
    version: Option[String] = None,
    errorOnUnknownArgument: Boolean = true) extends GenericOptionParser[C] {
  import OptionDefinition._

  type Def[A] = OptionDef[A]
  
  def this() = this(None, None, true)
  def this(programName: String) = this(Some(programName), None, true)
  def this(programName: String, version: String) = this(Some(programName), Some(version), true)
  def this(errorOnUnknownArgument: Boolean) = this(None, None, errorOnUnknownArgument)
  def this(programName: String, errorOnUnknownArgument: Boolean) =
    this(Some(programName), None , errorOnUnknownArgument)

  case class OptionDef[A: Read](
    id: Int,
    kind: OptionDefKind,
    name: String,
    _shortOpt: Option[Char] = None,
    _keyName: Option[String] = None,
    _valueName: Option[String] = None,
    _desc: String = "",
    _action: (A, C) => C = { (a: A, c: C) => c },
    _validations: Seq[A => Either[String, Unit]] = Seq(),
    _minOccurs: Int = 0,
    _maxOccurs: Int = 1) extends OptionDefinition[A, C] {
    /** Adds callback function. */
    def action(f: (A, C) => C): OptionDef[A] =
      copy(_action = (a: A, c: C) => { f(a, _action(a, c)) })
    /** Adds short option -x. */
    def shortOpt(x: Char): OptionDef[A] =
      copy(_shortOpt = Some(x))
    /** Requires the option to appear at least `n` times. */
    def minOccurs(n: Int): OptionDef[A] =
      copy(_minOccurs = n)
    /** Requires the option to appear at least once. */
    def required(): Def[A] = minOccurs(1)
    /** Chanages the option to be optional. */
    def optional(): Def[A] = minOccurs(0)
    /** Allows the argument to appear at most `n` times. */
    def maxOccurs(n: Int): OptionDef[A] =
      copy(_maxOccurs = n)
    /** Allows the argument to appear multiple times. */
    def unbounded(): OptionDef[A] = maxOccurs(UNBOUNDED)
    /** Adds description in the usage text. */
    def text(x: String): OptionDef[A] =
      copy(_desc = x)
    /** Adds value name used in the usage text. */
    def valueName(x: String): OptionDef[A] =
      copy(_valueName = Some(x))
    /** Adds key name used in the usage text. */
    def keyName(x: String): OptionDef[A] =
      copy(_keyName = Some(x))
    /** Adds key and value names used in the usage text. */
    def keyValueName(k: String, v: String): OptionDef[A] =
      keyName(k) valueName(v)
    /** Adds custom validation. */
    def validate(f: A => Either[String, Unit]) =
      copy(_validations = _validations :+ f)

    def callback: (A, C) => C = _action
    def getMinOccurs: Int = _minOccurs
    def getMaxOccurs: Int = _maxOccurs
  }

  protected def makeDef[A: Read](kind: OptionDefKind, name: String): Def[A] =
    OptionDef[A](id = generateId, kind = kind, name = name)

  /** adds an option invoked by `--name` that displays usage text.
   * @param name0 name of the option
   */
  def help(name0: String): OptionDef[Unit] =
    opt[Unit](name0) action { (x, c) => showUsage; c }
}
