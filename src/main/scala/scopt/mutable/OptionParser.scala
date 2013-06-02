package scopt.mutable

import scopt.generic._

/** scopt.mutable.OptionParser is instantiated within your object,
 * set up by an (ordered) sequence of invocations of 
 * the various builder methods such as
 * <a href="#opt(String,String,String,String,(String) ⇒ Unit):Unit"><code>opt</code></a> method or
 * <a href="#arg(String,String,(String) ⇒ Unit):Unit"><code>arg</code></a> method.
 * {{{
 * val parser = new OptionParser("scopt") {
 *   intOpt("f", "foo", "foo is an integer property", {v: Int => config.foo = v})
 *   opt("o", "output", "<file>", "output is a string property", {v: String => config.bar = v})
 *   booleanOpt("xyz", "xyz is a boolean property", {v: Boolean => config.xyz = v})
 *   keyValueOpt("l", "lib", "<libname>", "<filename>", "load library <libname>",
 *     {(key: String, value: String) => { config.libname = key; config.libfile = value } })
 *   arg("<singlefile>", "<singlefile> is an argument", {v: String => config.whatnot = v})
 *   // arglist("<file>...", "arglist allows variable number of arguments",
 *   //   {v: String => config.files = (v :: config.files).reverse })
 * }
 * if (parser.parse(args)) {
 *   // do stuff
 * }
 * else {
 *   // arguments are bad, usage message will have been displayed
 * }
 * }}}
 */
case class OptionParser(
        programName: Option[String],
        version: Option[String],
        errorOnUnknownArgument: Boolean) extends GenericOptionParser[Unit] {
  import OptionDefinition._

  def this() = this(None, None, true)
  def this(programName: String) = this(Some(programName), None, true)
  def this(programName: String, version: String) = this(Some(programName), Some(version), true)
  def this(errorOnUnknownArgument: Boolean) = this(None, None, errorOnUnknownArgument)
  def this(programName: String, errorOnUnknownArgument: Boolean) =
    this(Some(programName), None , errorOnUnknownArgument)
  
  val options = new scala.collection.mutable.ListBuffer[OptionDef[_]]

  case class OptionDef[A: Read](
    id: Int,
    kind: OptionDefKind,
    name: String,
    _shortOpt: Option[Char] = None,
    _keyName: Option[String] = None,
    _valueName: Option[String] = None,
    _desc: String = "",
    _action: (A => Unit) = { (a: A) => () },
    _minOccurs: Int = 0,
    _maxOccurs: Int = 1) extends OptionDefinition[A, Unit] {    
    /** Adds callback function. */
    def action(f: A => Unit): OptionDef[A] =
      updateOption(copy(_action = (a: A) => { _action(a); f(a) }))
    /** Adds short option -x. */
    def shortOpt(x: Char): OptionDef[A] =
      updateOption(copy(_shortOpt = Some(x)))
    /** Requires the option to appear at least `n` times. */
    def minOccurs(n: Int): OptionDef[A] =
      updateOption(copy(_minOccurs = n))
    /** Requires the option to appear at least once. */
    def required(): OptionDef[A] = minOccurs(1)
    /** Chanages the option to be optional. */
    def optional(): OptionDef[A] = minOccurs(0)
    /** Allows the argument to appear at most `n` times. */
    def maxOccurs(n: Int): OptionDef[A] =
      updateOption(copy(_maxOccurs = n))
    /** Allows the argument to appear multiple times. */
    def unbounded(): OptionDef[A] = maxOccurs(UNBOUNDED)
    /** Adds description in the usage text. */
    def text(x: String): OptionDef[A] =
      updateOption(copy(_desc = x))
    /** Adds value name used in the usage text. */
    def valueName(x: String): OptionDef[A] =
      updateOption(copy(_valueName = Some(x)))
    /** Adds key name used in the usage text. */
    def keyName(x: String): OptionDef[A] =
      updateOption(copy(_keyName = Some(x)))
    /** Adds key and value names used in the usage text. */
    def keyValueName(k: String, v: String): OptionDef[A] =
      keyName(k) valueName(v)
    def callback: (A, Unit) => Unit =
      { (a, c) => _action(a) }
    def getMinOccurs: Int = _minOccurs
    def getMaxOccurs: Int = _maxOccurs
  }

  /** parses the given `args`.
   * @return `true` if successful, `false` otherwise
   */
  def parse(args: Seq[String]): Boolean =
    parse(args, ()) match {
      case Some(x) => true
      case None    => false
    }

  protected def add[A: Read](option: OptionDef[A]): OptionDef[A] = {
    options += option
    option
  }

  protected def updateOption[A: Read](option: OptionDef[A]): OptionDef[A] = {
    val idx = options indexWhere { _.id == option.id }
    if (idx > -1) options(idx) = option
    else options += option

    option
  }

  /** adds an option invoked by `--name x`.
   * @param name0 name of the option
   */
  def opt[A: Read](name0: String): OptionDef[A] =
    add(OptionDef[A](id = generateId, kind = Opt, name = name0))

  /** adds an option invoked by `-x value` or `--name value`.
   * @param x name of the short option
   * @param name0 name of the option
   */
  def opt[A: Read](x: Char, name0: String): OptionDef[A] =
    opt[A](name0) shortOpt(x)

  /** adds an option invoked by `--name` that displays usage text.
   * @param name0 name of the option
   */
  def help(name0: String): OptionDef[Unit] =
    opt[Unit](name0) action {_ => showUsage}
  
  /** adds usage text. */
  def note(x: String) =
    add(OptionDef[Unit](id = generateId, kind = Sep, name = "", _desc = x))

  /** adds an argument invoked by and option without `-` or `--`.
   * @param name0 name in the usage text
   */  
  def arg[A: Read](name0: String): OptionDef[A] =
    add(OptionDef[A](id = generateId, kind = Arg, name = name0)) required()
}
