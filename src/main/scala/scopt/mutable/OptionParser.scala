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
  import GenericOptionParser._

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
    _description: Option[String] = None,
    _action: (A => Unit) = { (a: A) => () },
    _minOccurs: Int = 0,
    _maxOccurs: Int = 1) extends OptionDefinition[A, Unit] {    
    /** Adds callback function. */
    def action(f: A => Unit): OptionDef[A] =
      updateOption(copy(_action = (a: A) => { _action(a); f(a) }))
    /** Adds short option -x. */
    def shortOpt(x: Char): OptionDef[A] =
      updateOption(copy(_shortOpt = Some(x)))
    /** Allows the argument to appear at most `n` times. */
    def maxOccurs(n: Int): OptionDef[A] =
      updateOption(copy(_maxOccurs = n))
    /** Allows the argument to appear multiple times. */
    def unbounded(): OptionDef[A] = maxOccurs(UNBOUNDED)
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

  // -------- Defining options ---------------
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

  def help(name0: String): OptionDef[Unit] =
    opt[Unit](name0) action {_ => showUsage}
    
  // def separator(description: String) =
  //   add(new SeparatorDefinition(description))

  /** adds an argument invoked by and option without `-` or `--`.
   * @param name0 name in the usage text
   */  
  def arg[A: Read](name0: String): OptionDef[A] =
    add(OptionDef[A](id = generateId, kind = Arg, name = name0))

  // /** adds an optional argument invoked by an option without `-` or `--`.
  //  * @param name name in the usage text
  //  * @param description description in the usage text
  //  * @param action callback function
  //  */  
  // def argOpt(name: String, description: String, action: String => Unit) =
  //   add(new Argument(name, description, 0, 1,
  //     { (s: String, _) => action(s) }))

  // /** adds an optional list of arguments invoked by options without `-` or `--`.
  //  * @param name name in the usage text
  //  * @param description description in the usage text
  //  * @param action callback function
  //  */
  // def arglistOpt(name: String, description: String, action: String => Unit) =
  //   add(new Argument(name, description, 0, UNBOUNDED,
  //     { (s: String, _) => action(s) }))
}
