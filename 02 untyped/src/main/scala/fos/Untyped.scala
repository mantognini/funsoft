package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/**
 * This object implements a parser and evaluator for the
 *  untyped lambda calculus found in Chapter 5 of
 *  the TAPL book.
 */
object Untyped extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".")
  import lexical.Identifier

  /**
   * Term     ::= AbsOrVar { AbsOrVar }
   */
  def Term: Parser[Term] = {
    def variable = ident ^^ { Var(_) }
    def abstraction = "\\" ~> ident ~ ("." ~> Term) ^^ { case name ~ term => Abs(Var(name), term) }
    def parentheses = "(" ~> Term <~ ")"
    def vap = variable | abstraction | parentheses

    def vapsReducer(ts: List[Term]) = ts.reduceLeft { App(_, _) }

    rep1(vap) ^^ vapsReducer | failure("illegal start of term")
  }

  case class ParseException(e: String) extends Exception

  def parse(input: String) =
    parse_impl(new lexical.Scanner(input))

  def parse(input: java.io.InputStream) =
    parse_impl(new lexical.Scanner(StreamReader(new java.io.InputStreamReader(input))))

  private def parse_impl(tokens: lexical.Scanner) = phrase(Term)(tokens)

  def parseOrDie(input: String) = parse(input) match {
    case Success(ast, _) => ast
    case Failure(e, _) => throw new ParseException(e)
    case Error(e, _) => throw new ParseException(e)
  }

  // FV == Free Variable
  def FV(t: Term): Set[Var] = t match {
    case x @ Var(_) => Set(x)
    case Abs(x, t1) => FV(t1) - x
    case App(t1, t2) => FV(t1) ++ FV(t2)
  }

  // Alpha-conversion
  // Rename y in t
  def alpha(t: Term, y: Var): Term = {
    val oldName = y.name // "prefix"

    def findUsedNameWithPrefix: Set[String] = {
      def addIfPrefix(acc: Set[String], name: String): Set[String] = if (name != oldName && name.startsWith(oldName)) acc + name else acc

      def walk(cur: Term, acc: Set[String]): Set[String] = cur match {
        case Var(vn) => addIfPrefix(acc, vn)
        case Abs(Var(vn), t1) => walk(t1, addIfPrefix(acc, vn))
        case App(t1, t2) => walk(t2, walk(t1, acc))
      }

      walk(t, Set())
    }

    def convertToInteger(str: String): Int = try {
      str.toInt
    } catch {
      case e: java.lang.NumberFormatException => 0
    }

    // if v, v1, v2, .., vi, vj (j is not necessarily i + 1), then the selected name is v{j+1}. 
    def findFirstFreeName: String = {
      val taken = findUsedNameWithPrefix
      val indexes = taken map { i => convertToInteger(i.stripPrefix(oldName)) }
      val lastIndex = (indexes + 0) max

      oldName + (lastIndex + 1)
    }

    def rename(cur: Term, newName: String): Term = cur match {
      case Var(vn) if vn == oldName => Var(newName)
      case v @ Var(_) => v
      case l @ Abs(Var(vn), _) if vn == oldName => l
      case Abs(v, t1) => Abs(v, rename(t1, newName))
      case App(t1, t2) => App(rename(t1, newName), rename(t2, newName))
    }

    val newName = findFirstFreeName
    t match {
      case Abs(x, t) => Abs(Var(newName), rename(t, newName))
      case _ => throw new RuntimeException("Unexpected input term for alpha conversion")
    }
  }

  // Substitute
  // Note: unnecessary test are left in comment for reference
  def substitute(body: Term, x: Var, s: Term): Term = body match {
    // [x → s]x = s
    case y: Var if y == x => s

    // [x → s]y = y                     if y ≠ x
    case y: Var /*if y != x*/ => y

    // [x → s](λy. t) = λy . t          if y = x
    case l @ Abs(y, t) if y == x => l

    // [x → s](λy. t) = λy . [x → s]t   if y ≠ x and y ∉ FV(s)
    case Abs(y, t) if /*y != x &&*/ !FV(s).contains(y) => Abs(y, substitute(t, x, s))

    // [x → s](λy. t) = λy . [x → s]t   if y ≠ x and y ∈ FV(s)
    case l @ Abs(y, t) /*if y != x && FV(s).contains(y)*/ => substitute(alpha(l, y), x, s)
    //                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //                                                   This will end up in the case just above

    // [x → s](t1 t2) = ([x → s]t1 [x → s]t2)
    case App(t1, t2) => App(substitute(t1, x, s), substitute(t2, x, s))
  }

  /** Term 't' does not match any reduction rule. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /**
   * Normal order (leftmost, outermost redex first).
   *
   *  @param t the initial term
   *  @return  the reduced term
   */
  def reduceNormalOrder(t: Term): Term = {
    def reduce(t: Term): Option[Term] = t match {
      // No reduction left to be applied
      case Var(_) => None

      //      t1 → t1p
      // ------------------
      //  λx. t1 → λx. t1p
      case Abs(x, t1) => reduce(t1) match {
        case Some(t1p) => Some(Abs(x, t1p))
        case None => None
      }

      // Rule order: A then B then C
      // ----------
      //
      //        A                        B                     C
      //
      //     t1 → t1p                                       t2 → t2p
      // ---------------- && (λx. b) t2 → [x → t2] b && ----------------
      //  t1 t2 → t1p t2                                 t1 t2 → t1 t2p
      //
      // Because leftmost, outermost redex is always reduced first
      case App(t1, t2) => {
        def ruleA = reduce(t1) map { t1p => App(t1p, t2) }

        def ruleB = t1 match {
          case Abs(x, b) => Some(substitute(b, x, t2))
          case _ => None
        }

        def ruleC = reduce(t2) map { t2p => App(t1, t2p) }

        ruleA orElse ruleB orElse ruleC
      }
    }

    reduce(t) match {
      case Some(t) => t
      case None => throw NoRuleApplies(t)
    }
  }

  /** Call by value reducer. */
  def reduceCallByValue(t: Term): Term = {
    // Most languages use a call-by-value strategy, in which only outermost redexes 
    // are reduced and where a redex is reduced only when its right-hand side has 
    // already been reduced to a value (a function).

    // The call-by-value strategy is strict, in the sense that the arguments to 
    // functions are always evaluated, whether or not they are used by the body of 
    // the function. In contrast, lazy strategies such a call-by-name and call-by-need 
    // evaluate only the arguments that are actually used.

    def isValue(t: Term) = t match {
      case Abs(_, _) => true
      case _ => false
    }

    def reduce(t: Term): Option[Term] = t match {

      // No reduction left to be applied
      case Var(_) => None

      // Don't try to reduce body of abstraction
      case Abs(x, t1) => None

      // Rules: A then B then C
      // -----
      //
      //        A                    B                       C
      //
      //     t1 → t1p             t2 → t2p
      // ---------------- &&  ---------------- && (λx. b) v2 → [x → v2] b
      //  t1 t2 → t1p t2       v1 t2 → v1 t2p
      case App(t1, t2) =>

        def ruleA = reduce(t1) map { App(_, t2) }

        def ruleB = if (isValue(t1)) reduce(t2) map { App(t1, _) } else None

        def ruleC = t1 match {
          case Abs(x, b) if isValue(t2) => Some(substitute(b, x, t2))
          case _ => None
        }

        ruleA orElse ruleB orElse ruleC
    }

    reduce(t) match {
      case Some(t) => t
      case None => throw NoRuleApplies(t)
    }
  }

  /**
   * Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the method that reduces a term by one step.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] =
    try {
      val t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        Stream.cons(t, Stream.empty)
    }

  def main(args: Array[String]): Unit = {
    parse(System.in) match {
      case Success(trees, _) =>
        println("normal order: ")
        for (t <- path(trees, reduceNormalOrder))
          println(t)
        println("call-by-value: ")
        for (t <- path(trees, reduceCallByValue))
          println(t)

      case e =>
        println(e)
    }
  }
}
