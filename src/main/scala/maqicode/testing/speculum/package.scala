
package maqicode.testing

/** An assertion framework for lightweight expectytions.
 *
 *  The blocks under inspection must be idempotent
 *  because if an assert fails, the block is re-evaluated.
 *  That's OK, because nobody writes side-effecting tests.
 *
 * {{{
 *   inspect {
 *     val i = 7
 *     i == 2
 *   }
 * }}}
 * is rewritten to
 * {{{
 *   try {
 *     val i = 7
 *     assert(i == 2, 1)   // booleans wrapped in tagged asserts
 *   } catch {
 *     case e: AssertionError => {
 *       val tmps = ???
 *       // your block here again, to re-eval exprs for display
 *       val i = 7  
 *       if (e.tag == 1) {
 *         tmps += i; tmps += false
 *         throw new AssertionError(format(tmps), e)
 *         // reachable if i == 2 suddenly succeeds
 *       } else {
 *         i == 2  // local side effects are ok
 *       }
 *       require(e.tag > 1) // if assert at 1, shouldn't be here
 *     }
 *   }
 * }}}
 */
package object speculum {

  import scala.language.experimental.macros
  //import scala.reflect.macros.{ BlackboxContext => Context }
  import scala.reflect.macros.blackbox.Context
  import scala.collection.mutable.ListBuffer
  import scala.util.Properties

  def inspectImpl[A: c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._

    def debug(s: String, t: Tree) = if (isDebug) Console println s"$s ${showRaw(t)}"

    /** The column that begins the field to display this tree. */
    def anchorOf(t: Tree): Int = t match {
      case Apply(rec, _)     => anchorOf(rec)
      case TypeApply(rec, _) => anchorOf(rec)
      case _                 => t.pos.column
    }
    /** Wrap booleans in asserts. */
    def wrapped(stats: List[Tree]): List[Tree] = stats.zipWithIndex map { case (t, i) =>
      if (t.tpe <:< typeOf[Boolean]) q"assert($t, $i)"
      else t
    }
    /** Generate the hander to replay the code, capturing intermediate values
     *  for the error message.
     */
    def handled(stats: List[Tree]) = {
      // generate the fancy assert message
      def display(t: Tree): Tree = {
        // modified tree to eval the result and store intermediate values
        val eval: Tree = {
          def enter(x: Tree): Tree = {
            // register for inspection
            def register(tmp: Tree): Tree = {
              def typee = Literal(Constant(x.tpe.typeSymbol.toString))
              if (x.tpe.typeSymbol.isType) q"""register($tmp, $typee, ${anchorOf(x)})"""
              else tmp
            }
            debug("Entering", x)
            x match {
              case Apply(rec, args) =>
                val rr = enter(rec)
                val as = args map ((z: Tree) => enter(z))
                register(q"""$rr(..$as)""")
              case TypeApply(rec, args) =>
                val rr = enter(rec)
                register(TypeApply(rr, args))
              case _: Select if x.symbol.isModule => x
              case Select(qual, name)   =>
                val qq = enter(qual)
                q"$qq.$name"
              case Literal(_) | New(_)  => x
              case _                    => register(x)
            }
          }
          enter(t)
        }
        debug("Registered", eval)

        // code to invoke the eval and generate the explanation
        def showing(x: Tree, margin: Int, offset: Int): Tree = {
            //import scala.tools.testing.speculum._
          q"""{
            import maqicode.testing.speculum._
            tmps.clear()
            val b = $eval
            if (b) Console println "Eval is unexpectedly true!"
            format(tmps.toList, $margin, $offset)
          }"""
        }

        val lincol = s"[${t.pos.line}:${t.pos.column}]: "
        val header = Literal(Constant(f"%n$lincol${t.pos.lineContent.trim}%n"))
        val margin = lincol.size
        val offset = t.pos.lineContent.prefixLength(_.isSpaceChar)
        q"""{
          val $$res  = ${ showing(t, margin, offset) }
          $header + $$res
        }"""
      }
      // re-eval each statement.
      // the booleans are checked for our failure, which we reassert
      def inspector = stats.zipWithIndex flatMap { case (t, i) =>
        debug("Inspecting", t)
        val eval =
          if (t.tpe <:< typeOf[Boolean])
            q"""if ($$failure == $i) {
                  val formatted = ${display(t)}
                  assert(false, formatted)
                } else $t"""
          else t
        List(eval, q"""require($$failure > $i, "" + $$failure + " > " + $i)""")
      }
      // peel off what failed, then look for it
      cq"""e: AssertionError =>
        val $$failr = ".* (\\d+)".r
        val $$failure = e.getMessage match {
          case $$failr(i) => i.toInt
          case _ => throw new IllegalStateException
        }
        import scala.collection.mutable.ListBuffer
        val tmps = new ListBuffer[(Any, String, Int)]
        def register[A](a: A, s: String, i: Int): A = {
          tmps += Tuple3(a, s, i)
          a
        }
        ..$inspector
        throw e
      """
    }
    def process(stats: List[Tree]): (List[Tree], CaseDef) =
      (wrapped(stats map (_.duplicate)), handled(stats map (_.duplicate)))

    val (block, handler) = a.tree match {
      case Block(stats, res) => process(stats :+ res)
      case _                 => process(a.tree :: Nil)
    }
    val q = q"try { ..$block } catch { case $handler }"
    //def clean(t: Tree) = t.setPos(t.pos.makeTransparent)
    //q foreach (t => clean(t))
    debug("Emitting", q)
    c.Expr[Unit](q)
  }

  /** Inspect the given expression.
   *
   *  Given a boolean-typed expression or a block with child
   *  expressions that are boolean-typed, those booleans are
   *  wrapped in asserts.
   *
   *  Any assertion errors are caught, and the block is
   *  replayed in order to inspect the values that were produced.
   */
  def inspect[A](a: A): Unit = macro inspectImpl[A]

  type Entry = (Any, String, Int)

  /** Format the tmps as in expecty.
   *
   *  Only the last (uppermost) value at a column is retained.
   *
   *  @param tmps a list of (value, type, column)
   *  @param margin width of the left margin
   *  @param offset strippage at beginning of line as displayed
   */
  def format(tmps: List[Entry], margin: Int, offset: Int): String = {
    import scala.runtime.ScalaRunTime.stringOf
    val uniques = {
      def onlies(xs: List[Entry]): List[Entry] = xs match {
        case h :: rest => h :: onlies(rest filter (_._3 != h._3))
        case _ => xs
      }
      onlies(tmps.reverse).reverse
    }
    val ordered = uniques sortWith ((t1,t2) => t1._3 < t2._3)
    val texts   = ordered map { case (v, _, i) => stringOf(v) }
    val cols    = ordered map { case (v, _, i) => i }
    val widths  = (0 until cols.size) map (i => (if (i+1 < cols.size) cols(i+1) else 100) - cols(i))
    val biggies = (0 until cols.size) map (i => texts(i).length >= widths(i) || i == cols.size - 1)
    val levels  = (0 until cols.size) map { i =>
      if (biggies(i)) (biggies drop i).reverse count identity
      else 1
    }
    val fmts    = widths map (w => s"%-${w}.${w}s")
    val leader  = " " * (margin + cols(0) - offset - 1)
    if (isDebug) {
      Console println s"cols $cols"
      Console println s"widths $widths"
      Console println s"texs $texts"
      Console println s"levels $levels"
      Console println s"fmts $fmts"
    }
    val sb    = new StringBuilder
    0 to levels.max foreach { i =>
      sb append leader append (0 until cols.size map { j =>
        val t =
          if (levels(j) == i) texts(j)
          else if (levels(j) > i) "|"
          else ""
        def eol = j >= cols.size - 1 || levels(j+1) < i
        if (eol) t else (fmts(j) format t)
      }).mkString append "\n"
    }
    sb.toString
  }

  private[speculum] val isDebug = Properties propOrFalse "scala.speculum.debug"
}
